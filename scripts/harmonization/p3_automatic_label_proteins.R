#Author: Aaron Yerke (aaronyerke@gmail.com)
#Script for labeling the protein sources based off of rudimentary text mining
#And adding HEI values
#HEI_conversion_table_LEO wide.xlsx was hand made by lauren for checking our work.
#Need to seperate this script into two steps to check out results.

rm(list = ls()) #clear workspace

#### Loading libraries and data ####
#read in libraries
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("ggplot2", quietly = TRUE))  BiocManager::install("ggplot2")
library("ggplot2")
if (!requireNamespace("readxl", quietly = TRUE))  BiocManager::install("readxl")
library("readxl")
if (!requireNamespace("data.table", quietly = TRUE))  BiocManager::install("data.table")
library("data.table")
if (!requireNamespace("optparse", quietly = TRUE)) BiocManager::install("optparse")
library("optparse")
print("Libraries are loaded.")

#### Parse command line arguments ####
option_list <- list(
  optparse::make_option(c("-p", "--HEI_conv"), type="character",
                        default = "data/diet/nutrition_data/HEI Pivot table_modified.xlsx",
                        help="path of HEI conversion table"),
  optparse::make_option(c("-i", "--input"), type="character",
                        default = "data/diet/nutrition_data/combined_esha_studies.tsv",
                        help='Dietary data with the following columns: "Study"             "Intervention"      "Day"               "Meal"             
 "Recipe"            "Item name"         "Quantity"          "Measure"          
  "Wgt (g)"           "Cals (kcal)", and other nutritional features'),
  optparse::make_option(c("-o", "--out_prefix"), type="character",
                        default = "",
                        help="Prefix to show which group of data this is, since 
                        we didn't add to the first dataset, this is empty.")
);
opt_parser <- optparse::OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

print("Commandline arguments:")
print(opt)

#### Establish directory layout and other constants ####
data_dir <- file.path("data", "diet", "nutrition_data")
labeled_file <- "HEI_conversion_table_LEO wide.xlsx"
labeled_sheet <- 2
my_excel <- readxl::read_excel(file.path(data_dir, labeled_file), sheet = labeled_sheet, 
                               trim_ws = F, na = c("", "NA"))
print("Removing dupiclated rows")
my_excel <- my_excel[!duplicated(my_excel),]


df <- data.table::fread(file = file.path(data_dir, "HEI_conversion_table_wide.tsv"),
                        check.names = FALSE)

#### Dictionaries for classifying food ####
protein_source_pos <- list(
                           "beef" = c("beef"),
                           "turkey" = c("turkey"),
                           "lamb" = c("lamb", "mutton", "sheep"),
                           "chicken" = c("chicken","chix"),
                           "pork" = c("pork", " ham ", "bacon", "fatback", "sausage"),
                           "seafood" = c("shrimp", "crab", "fish", "lobster", "salmon", "tuna", "cod")
                           #Can ignore mprocessed
                           # "mprocessed" = c("steak", "chops", "ground", "baked", 
                           #                  "broiled", "roast", "fillet", "saute", 
                           #                  "sauté")
                           # "plant-based" = c("nuts", "almond", "bean")
                           # "dairy" = c("yogurt", "yoghurt", "milk", "cheese"),
                           )#strings to select protein source
protein_source_cancel <- list("beef" = c("impossible", "beyond", "broth", "base", "stock"),
                              "pork" = c("impossible", "beyond", "base", "stock", "turkey"),
                              "lamb" = c("broth", "base", "stock"),
                              "chicken" = c("impossible", "beyond", "broth", "base", "stock", "seasoning"),
                              "turkey" = c("impossible", "beyond", "hill", "broth", "base", "stock"),
                              "seafood" = c("goldfish")
                              )#strings to cancel protein source selection

processed_indicators <- c("sausage", "sandwich", "bratworst", "smoked",
                "hormel", "bologna", "cured", "deli", "lunchmeat", "hot dog",
                "salted", "pickled", "bacon", "BBQ", "barbeque", "hotdog",
                "patties", "nuggets", "tenders", "jerky", "jerked")

#df to fill out
pro_source_prediction <- data.frame("item" = character(length = nrow(my_excel)), 
                                "beef" = rep(0, nrow(my_excel)),
                                "pork" = rep(0, nrow(my_excel)),
                                "lamb" = rep(0, nrow(my_excel)),
                                "chicken" = rep(0, nrow(my_excel)),
                                "turkey" = rep(0, nrow(my_excel)),
                                "seafood" = rep(0, nrow(my_excel)),
                                "meat" = logical(length = nrow(my_excel)),
                                "processed" = logical(length = nrow(my_excel)))
#### Cycle through items and label them ####
for (rw in 1:nrow(my_excel)){
  itm <- my_excel$item[rw]
  pro_source_prediction$item[rw] <- itm
  #START label protein source
  meat_found <- c()
  for (p_source_num in 1:length(protein_source_pos)){
    p_source <- names(protein_source_pos)[p_source_num]
    indicators <- protein_source_pos[p_source_num]
    cancels <- protein_source_cancel[p_source]
    for (ind in unlist(indicators)){
      if (grepl(pattern = ind, x = itm, ignore.case = TRUE)){#find pos indicator
        if (!grepl(paste(unlist(cancels), collapse = "|"),x = itm, ignore.case = TRUE)){#find cancel factors
          meat_found <- unique(c(meat_found, p_source)) #Unique b/c we may add same source twice
        }
      }
    }#END FOR LOOP
  }
  if (length(meat_found) > 0){
    pro_source_prediction$meat[rw] <- TRUE
    food_proportion <- 1/length(meat_found)# Limitation: assumes equal split
    for (fnd_itm in meat_found) {
      pro_source_prediction[rw,fnd_itm] <- food_proportion
    }
    # print(paste(itm, meat_found, sep = ","))
  }
  # END label protein source
  # START label "processed"
  for (ind in processed_indicators){
    if (grepl(pattern = ind, x = itm, ignore.case = TRUE)){#find pos indicator
      # print(paste(itm, "is", ind))
      pro_source_prediction$processed[rw] <- TRUE
    }
  }#END FOR LOOP
}

#### Create Columns for red meat, poultry, and m_processed meat in output ####
my_excel$beef <- pro_source_prediction$beef
my_excel$pork <- pro_source_prediction$pork
my_excel$chicken <- pro_source_prediction$chicken
my_excel$turkey <- pro_source_prediction$turkey
my_excel$seafood <- pro_source_prediction$seafood
my_excel$processed <- pro_source_prediction$processed
my_excel$red_meat <- pro_source_prediction$beef + pro_source_prediction$pork
my_excel$poultry <- pro_source_prediction$chicken + pro_source_prediction$turkey
my_excel$meat <- pro_source_prediction$meat

#Min processed meat
my_excel$mproc_meat <- pro_source_prediction$meat * !pro_source_prediction$processed
my_excel$mproc_beef <- pro_source_prediction$beef * !pro_source_prediction$processed
my_excel$mproc_pork <- pro_source_prediction$pork * !pro_source_prediction$processed
my_excel$mproc_chicken <- pro_source_prediction$chicken * !pro_source_prediction$processed
my_excel$mproc_turkey <- pro_source_prediction$turkey * !pro_source_prediction$processed

#processed meat
my_excel$proc_meat <- pro_source_prediction$meat * pro_source_prediction$processed
my_excel$proc_beef <- pro_source_prediction$beef * pro_source_prediction$processed
my_excel$proc_pork <- pro_source_prediction$pork * pro_source_prediction$processed
my_excel$proc_chicken <- pro_source_prediction$chicken * pro_source_prediction$processed
my_excel$proc_turkey <- pro_source_prediction$turkey * pro_source_prediction$processed

#### Compare meat processing and type results to Lauren's manual labeling ####
#Build from hand labeled first
#labeling just the type of meat and whether it is processed or min processed
hand_label_type_cols <- c("Beef", "Pork","Chicken","Turkey")
#labeling the type of meat 
#columns
hand_lab_proc_cols <- sort(names(my_excel)[grepl("^Proc_", names(my_excel))])
hand_lab_Mproc_cols <- sort(names(my_excel)[grepl("^Mproc_", names(my_excel))])

hand_lab_cols <- c(hand_label_type_cols,
                   hand_lab_Mproc_cols,
                   hand_lab_proc_cols)
#columns to dataframes
hand_lab <- my_excel[,hand_lab_cols]

# Now build from hand labeled, taking advantage of the fact that Lauren's start
# with a capital and mine start with a lowercase, but spelled the same.
predicted_lab <- my_excel[,tolower(hand_lab_cols)]

mismatches <- data.frame(which(hand_lab != predicted_lab, arr.ind=TRUE))

print("THE FOLLOWING ARE DIFFERENCES IN THE HAND LABELED MEAT TYPE VERSE PROGRAM LABELED")
for (m_row in unique(mismatches$row)) {
  # m_row <- mismatches$row[mism]
  print(paste(m_row, "hand first"))
  print(my_excel$item[m_row])
  print(hand_lab[m_row,])
  print(predicted_lab[m_row,])
}
# [1] "12 hand first"
# [1] "Bologna- beef and pork (BERKS)"
# # A tibble: 1 × 12
# Beef  Pork Chicken Turkey Mproc_Beef Mproc_Chicken Mproc_Pork Mproc_Turkey Proc_Beef Proc_Chicken Proc_pork Proc_Turkey
# <dbl> <dbl>   <dbl>  <dbl>      <dbl>         <dbl>      <dbl>        <dbl>     <dbl>        <dbl>     <dbl>       <dbl>
#   1     1     1       0      0          0             0          0            0         1            0         1           0
# # A tibble: 1 × 12
# beef  pork chicken turkey mproc_beef mproc_chicken mproc_pork mproc_turkey proc_beef proc_chicken proc_pork proc_turkey
# <dbl> <dbl>   <dbl>  <dbl>      <dbl>         <dbl>      <dbl>        <dbl>     <dbl>        <dbl>     <dbl>       <dbl>
#   1   0.5   0.5       0      0          0             0          0            0       0.5            0       0.5           0
# [1] "13 hand first"
# [1] "Bologna- Beef and Pork (BERKS)"
# # A tibble: 1 × 12
# Beef  Pork Chicken Turkey Mproc_Beef Mproc_Chicken Mproc_Pork Mproc_Turkey Proc_Beef Proc_Chicken Proc_pork Proc_Turkey
# <dbl> <dbl>   <dbl>  <dbl>      <dbl>         <dbl>      <dbl>        <dbl>     <dbl>        <dbl>     <dbl>       <dbl>
#   1     1     1       0      0          0             0          0            0         1            0         1           0
# # A tibble: 1 × 12
# beef  pork chicken turkey mproc_beef mproc_chicken mproc_pork mproc_turkey proc_beef proc_chicken proc_pork proc_turkey
# <dbl> <dbl>   <dbl>  <dbl>      <dbl>         <dbl>      <dbl>        <dbl>     <dbl>        <dbl>     <dbl>       <dbl>
#   1   0.5   0.5       0      0          0             0          0            0       0.5            0       0.5           0
# [1] "78 hand first"
# [1] "turkey, ground, raw, 15% fat"
# # A tibble: 1 × 12
# Beef  Pork Chicken Turkey Mproc_Beef Mproc_Chicken Mproc_Pork Mproc_Turkey Proc_Beef Proc_Chicken Proc_pork Proc_Turkey
# <dbl> <dbl>   <dbl>  <dbl>      <dbl>         <dbl>      <dbl>        <dbl>     <dbl>        <dbl>     <dbl>       <dbl>
#   1     0     0       0      1          0             0          0            0         0            0         0           1
# # A tibble: 1 × 12
# beef  pork chicken turkey mproc_beef mproc_chicken mproc_pork mproc_turkey proc_beef proc_chicken proc_pork proc_turkey
# <dbl> <dbl>   <dbl>  <dbl>      <dbl>         <dbl>      <dbl>        <dbl>     <dbl>        <dbl>     <dbl>       <dbl>
#   1     0     0       0      1          0             0          0            1         0            0         0           0


data.table::fwrite(my_excel, file = file.path(data_dir, "HEI_with_proportions_long.tsv"), 
                   sep = "\t", row.names = F)
openxlsx::write.xlsx(my_excel, file = file.path(data_dir, "HEI_with_proportions_long.xlsx"))

#### Update naming scheme of long combined esha studies  ####
# Instructions from Lauren (first 2 done in excel)
# 1. Rename variables: please replace row 2 var names with what I put in row 1. I am creating a codebook that will describe each of these named vars (full name, unit, etc.) that will be used across all studies.
# 2. Drop unwanted variables: drop all red columns
# 3. HEI vars: it seems that HEI_TotalFruit, HEI_WholeFruit, and HEI_WholeGrains are missing (highlighted in yellow). Please add.
  # a. Read in file with new columns
  # b. add columns
# 4. Add meat vars (var names in attached sheet) so that quantity equals column J (grams), unless the proportions need to be split (like beef and pork hot dogs split the amount in column J into 50/50).
# 5. For HEI and meat vars: if there is no value for that var, like redmeat=false for pancakes, then redmeat should = 0. Same for HEI, if there are no veg HEI component of peaches, then HEI_TotalVeg=0 (i.e., most of the HEI vals will be 0. See the attached FPED sheet as an example).
# 6. Follow Kai's instructions to try to create a dataset from SR legacy and let's discuss how we can impute the data during our next meeting. I think we need the data set up similar to the fped sheet that is attached but with nutrients, not food groups.
  # Do this in seperate script
# 7. Lauren to do after this: spot check HEI and meat amounts to see if conversion factors are okay.

esha_studies <- readxl::read_excel(file.path(data_dir, "esha_combined_meats_HEI_vals_LEO_noRed.xlsx"), 
                                   trim_ws = F, na = c("", "NA"))
esha_studies <- type.convert(esha_studies, as.is = TRUE)#automatically reset incorrectly classified column types

#### Use HEI values to convert g to oz  ####
# Organize meat type of column names
base_col_names <- c("beef", "chicken", "pork", "turkey", "meat")
agg_columns <- unlist(lapply(base_col_names, function(x){
  c(x, paste0(x, "_proc"), paste0(x, "_min_proc"))
}))

#### Add food aggregation columns to output df  ####
for (ag in agg_columns){
  esha_studies[,ag] <- vector(length = nrow(esha_studies), mode = "double")
}

#HEI vars
esha_studies$HEI_gb_cup <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$HEI_dairy_oz <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$HEI_total_pro_oz <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$HEI_SeaPlantPro_oz <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$HEI_refinedG_oz <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$HEI_addedSug_tsp <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$HEI_total_veg_cup <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$HEI_total_total_fruit_cup <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$HEI_total_wholeFruit_cup <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$HEI_total_wholeGrains_cup <- vector(length = nrow(esha_studies), mode = "double")

for (rw in 1:nrow(esha_studies)){
  my_item <- esha_studies$Item_Name[rw]
  grms <- as.numeric(esha_studies$Gram_weight[rw])
  HEI_index <- which(my_excel$item == my_item)

  esha_studies$beef[rw] <- grms * as.numeric(my_excel$beef[HEI_index])
  esha_studies$pork[rw] <- grms * as.numeric(my_excel$pork[HEI_index])
  esha_studies$chicken[rw] <- grms * as.numeric(my_excel$chicken[HEI_index])
  esha_studies$turkey[rw] <- grms * as.numeric(my_excel$turkey[HEI_index])
  esha_studies$seafood[rw] <- grms * as.numeric(my_excel$seafood[HEI_index])
  esha_studies$processed[rw] <- my_excel$processed[HEI_index]
  esha_studies$red_meat[rw] <- grms * as.numeric(my_excel$red_meat[HEI_index])
  esha_studies$poultry[rw] <- grms * as.numeric(my_excel$poultry[HEI_index])
  esha_studies$meat[rw] <- grms * as.numeric(my_excel$meat[HEI_index])
  
  #Min processed meat
  esha_studies$meat_min_proc[rw] <- grms * as.numeric(my_excel$mproc_meat[HEI_index])
  esha_studies$beef_min_proc[rw] <- grms * as.numeric(my_excel$mproc_beef[HEI_index])
  esha_studies$pork_min_proc[rw] <- grms * as.numeric(my_excel$mproc_pork[HEI_index])
  esha_studies$chicken_min_proc[rw] <- grms * as.numeric(my_excel$mproc_chicken[HEI_index])
  esha_studies$turkey_min_proc[rw] <- grms * as.numeric(my_excel$mproc_turkey[HEI_index])
  
  #processed meat
  esha_studies$meat_proc[rw] <- grms * as.numeric(my_excel$proc_meat[HEI_index])
  esha_studies$beef_proc[rw] <- grms * as.numeric(my_excel$proc_beef[HEI_index])
  esha_studies$pork_proc[rw] <- grms * as.numeric(my_excel$proc_pork[HEI_index])
  esha_studies$chicken_proc[rw] <- grms * as.numeric(my_excel$proc_chicken[HEI_index])
  esha_studies$turkey_proc[rw] <- grms * as.numeric(my_excel$turkey[HEI_index])
  
  #HEI var
  esha_studies$HEI_gb_cup[rw] <- grms / as.numeric(my_excel$HEI_GreensBeans_Conv[HEI_index])
  esha_studies$HEI_dairy_oz[rw] <- grms / as.numeric(my_excel$HEI_Dairy_Conv[HEI_index])
  esha_studies$HEI_total_pro_oz[rw] <- grms / as.numeric(my_excel$HEI_TotalProtein_Conv[HEI_index])
  esha_studies$HEI_SeaPlantPro_oz[rw] <- grms / as.numeric(my_excel$HEI_SeaPlantPro_Conv[HEI_index])
  esha_studies$HEI_refinedG_oz[rw] <- grms / as.numeric(my_excel$HEI_RefinedGrains_Conv[HEI_index])
  esha_studies$HEI_addedSug_tsp[rw] <- grms / as.numeric(my_excel$HEI_AddedSugar_Conv[HEI_index])
  esha_studies$HEI_total_veg_cup[rw] <- grms / as.numeric(my_excel$HEI_TotalVeg_Conv[HEI_index])
  esha_studies$HEI_total_total_fruit_cup[rw] <- grms / as.numeric(my_excel$HEI_TotalFruit_Conv[HEI_index])
  esha_studies$HEI_total_wholeFruit_cup[rw] <- grms / as.numeric(my_excel$HEI_WholeFruit_Conv[HEI_index])
  esha_studies$HEI_total_wholeGrains_cup[rw] <- grms / as.numeric(my_excel$HEI_WholeGrains_Conv[HEI_index])
}

# processed_meat <- esha_studies$processed * esha_studies$meat
# esha_studies$processed <- processed_meat

# set missing values to NA, as per Lauren's instruction
esha_studies[is.na(esha_studies)] <- 0

data.table::fwrite(esha_studies, file = file.path(data_dir, "esha_combined_meats_HEI_vals28Jun2025.tsv"),
                   sep = "\t", row.names = F)
openxlsx::write.xlsx(esha_studies, file = file.path(data_dir, "esha_combined_meats_HEI_vals28Jun2025.xlsx"))

print("Reached end of R script!")
