#Author: Aaron Yerke (aaronyerke@gmail.com)
#Script for labeling the protein sources based off of rudimentary text mining
#And adding HEI values
#HEI_conversion_table_LEO wide.xlsx was hand made by Lauren for checking our work.
#Need to separate this script into two steps to check out results.

rm(list = ls()) #clear workspace

#### Loading libraries and data ####
#read in libraries
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("ggplot2", quietly = TRUE)) BiocManager::install("ggplot2")
library("ggplot2")
if (!requireNamespace("readxl", quietly = TRUE)) BiocManager::install("readxl")
library("readxl")
if (!requireNamespace("data.table", quietly = TRUE)) BiocManager::install("data.table")
library("data.table")
if (!requireNamespace("optparse", quietly = TRUE)) BiocManager::install("optparse")
library("optparse")
print("Libraries are loaded.")

#### Parse command line arguments ####
option_list <- list(
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

##### Load hand labeled and machine label files and remove duplicated rows #####
hand_lab <- readxl::read_excel(file.path(data_dir, labeled_file), sheet = labeled_sheet, 
                               trim_ws = F, na = c("", "NA"))
hand_lab <- hand_lab[!duplicated(hand_lab),]

conv_table <- data.table::fread(file = file.path(data_dir, "HEI_conversion_table_wide.tsv"),
                                check.names = FALSE)
conv_table <- conv_table[!duplicated(conv_table),]

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
pro_source_prediction <- data.frame("item" = character(length = nrow(conv_table)), 
                                "beef" = rep(0, nrow(conv_table)),
                                "pork" = rep(0, nrow(conv_table)),
                                "lamb" = rep(0, nrow(conv_table)),
                                "chicken" = rep(0, nrow(conv_table)),
                                "turkey" = rep(0, nrow(conv_table)),
                                "seafood" = rep(0, nrow(conv_table)),
                                "meat" = logical(length = nrow(conv_table)),
                                "processed" = logical(length = nrow(conv_table)))
#### Cycle through items and label them ####
for (rw in 1:nrow(conv_table)){
  itm <- conv_table$item[rw]
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
conv_table$beef <- pro_source_prediction$beef
conv_table$pork <- pro_source_prediction$pork
conv_table$chicken <- pro_source_prediction$chicken
conv_table$turkey <- pro_source_prediction$turkey
conv_table$seafood <- pro_source_prediction$seafood
conv_table$processed <- pro_source_prediction$processed
conv_table$red_meat <- pro_source_prediction$beef + pro_source_prediction$pork
conv_table$poultry <- pro_source_prediction$chicken + pro_source_prediction$turkey
conv_table$meat <- pro_source_prediction$meat

#Min processed meat
conv_table$mproc_meat <- pro_source_prediction$meat * !pro_source_prediction$processed
conv_table$mproc_beef <- pro_source_prediction$beef * !pro_source_prediction$processed
conv_table$mproc_pork <- pro_source_prediction$pork * !pro_source_prediction$processed
conv_table$mproc_chicken <- pro_source_prediction$chicken * !pro_source_prediction$processed
conv_table$mproc_turkey <- pro_source_prediction$turkey * !pro_source_prediction$processed

#processed meat
conv_table$proc_meat <- pro_source_prediction$meat * pro_source_prediction$processed
conv_table$proc_beef <- pro_source_prediction$beef * pro_source_prediction$processed
conv_table$proc_pork <- pro_source_prediction$pork * pro_source_prediction$processed
conv_table$proc_chicken <- pro_source_prediction$chicken * pro_source_prediction$processed
conv_table$proc_turkey <- pro_source_prediction$turkey * pro_source_prediction$processed

#### Compare meat processing and type results to Lauren's manual labeling ####
#Build from hand labeled first
#labeling just the type of meat and whether it is processed or min processed
hand_label_type_cols <- c("item", "Beef", "Pork","Chicken","Turkey")
#labeling the type of meat 
#columns
hand_lab_proc_cols <- sort(names(hand_lab)[grepl("^Proc_", names(hand_lab))])
hand_lab_Mproc_cols <- sort(names(hand_lab)[grepl("^Mproc_", names(hand_lab))])

hand_lab_cols <- c(hand_label_type_cols,
                   hand_lab_Mproc_cols,
                   hand_lab_proc_cols)
#columns to dataframes
hand_lab <- hand_lab[,hand_lab_cols]

# Now build from hand labeled, taking advantage of the fact that Lauren's start
# with a capital and mine start with a lowercase, but spelled the same.
# predicted_lab <- conv_table[,tolower(hand_lab_cols)]
# 
# mismatches <- data.frame(which(hand_lab != predicted_lab, arr.ind=TRUE))
# 
# print("THE FOLLOWING ARE DIFFERENCES IN THE HAND LABELED MEAT TYPE VERSE PROGRAM LABELED")
# for (m_row in unique(mismatches$row)) {
#   # m_row <- mismatches$row[mism]
#   print(paste(m_row, "hand first"))
#   print(conv_table$item[m_row])
#   print(hand_lab[m_row,])
#   print(predicted_lab[m_row,])
# }
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


data.table::fwrite(conv_table, file = file.path(data_dir, "HEI_with_proportions_long.tsv"), 
                   sep = "\t", row.names = F)
openxlsx::write.xlsx(conv_table, file = file.path(data_dir, "HEI_with_proportions_long.xlsx"))

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

#### Add HEI amounts to HEI foods and meat categories in output df  ####
for (ag in agg_columns){
  esha_studies[,ag] <- vector(length = nrow(esha_studies), mode = "double")
}

#HEI vars
esha_studies$HEI_gb_cup <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$HEI_dairy_cup <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$HEI_total_pro_oz <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$HEI_SeaPlantPro_oz <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$HEI_refinedG_oz <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$HEI_addedSug_tsp <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$HEI_total_veg_cup <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$HEI_total_fruit_cup <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$HEI_total_wholeFruit_cup <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$HEI_total_wholeGrains_oz <- vector(length = nrow(esha_studies), mode = "double")

esha_studies$HEI_gb_cup_convs <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$HEI_dairy_cup_convs <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$HEI_total_pro_oz_convs <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$HEI_SeaPlantPro_oz_convs <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$HEI_refinedG_oz_convs <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$HEI_addedSug_tsp_convs <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$HEI_total_veg_cup_convs <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$HEI_total_fruit_cup_convs <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$HEI_total_wholeFruit_cup_convs <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$HEI_total_wholeGrains_oz_convs <- vector(length = nrow(esha_studies), mode = "double")

##### Loop through each item and add HEI values #####

for (rw in 1:nrow(esha_studies)){
  my_item <- esha_studies$Item_Name[rw]
  grms <- as.numeric(esha_studies$Gram_weight[rw])
  if (my_item %in% conv_table$item){
    HEI_index <- which(conv_table$item == my_item)
    
    esha_studies$beef[rw] <- grms * conv_table$beef[HEI_index]
    esha_studies$pork[rw] <- grms * conv_table$pork[HEI_index]
    esha_studies$chicken[rw] <- grms * conv_table$chicken[HEI_index]
    esha_studies$turkey[rw] <- grms * conv_table$turkey[HEI_index]
    # esha_studies$seafood[rw] <- grms * conv_table$seafood[HEI_index]
    # esha_studies$processed[rw] <- conv_table$processed[HEI_index]
    # esha_studies$red_meat[rw] <- grms * conv_table$red_meat[HEI_index]
    # esha_studies$poultry[rw] <- grms * conv_table$poultry[HEI_index]
    esha_studies$meat[rw] <- grms * conv_table$meat[HEI_index]
    
    #Min processed meat
    esha_studies$meat_min_proc[rw] <- grms * conv_table$mproc_meat[HEI_index]
    esha_studies$beef_min_proc[rw] <- grms * conv_table$mproc_beef[HEI_index]
    esha_studies$pork_min_proc[rw] <- grms * conv_table$mproc_pork[HEI_index]
    esha_studies$chicken_min_proc[rw] <- grms * conv_table$mproc_chicken[HEI_index]
    esha_studies$turkey_min_proc[rw] <- grms * conv_table$mproc_turkey[HEI_index]
    
    #processed meat
    esha_studies$meat_proc[rw] <- grms * conv_table$proc_meat[HEI_index]
    esha_studies$beef_proc[rw] <- grms * conv_table$proc_beef[HEI_index]
    esha_studies$pork_proc[rw] <- grms * conv_table$proc_pork[HEI_index]
    esha_studies$chicken_proc[rw] <- grms * conv_table$proc_chicken[HEI_index]
    esha_studies$turkey_proc[rw] <- grms * conv_table$turkey[HEI_index]
    
    #HEI var
    esha_studies$HEI_gb_cup[rw] <- grms / conv_table$HEI_GreensBean_Conv[HEI_index]
    esha_studies$HEI_dairy_cup[rw] <- grms / conv_table$HEI_Dairy_Conv[HEI_index]
    esha_studies$HEI_total_pro_oz[rw] <- grms / conv_table$HEI_TotalProtein_Conv[HEI_index]
    esha_studies$HEI_SeaPlantPro_oz[rw] <- grms / conv_table$HEI_SeaPlantPro_Conv[HEI_index]
    esha_studies$HEI_refinedG_oz[rw] <- grms / conv_table$HEI_RefinedGrains_Conv[HEI_index]
    esha_studies$HEI_addedSug_tsp[rw] <- grms / conv_table$HEI_AddedSugar_Conv[HEI_index]
    esha_studies$HEI_total_veg_cup[rw] <- grms / conv_table$HEI_TotalVeg_Conv[HEI_index]
    esha_studies$HEI_total_fruit_cup[rw] <- grms / conv_table$HEI_TotalFruit_Conv[HEI_index]
    esha_studies$HEI_total_wholeFruit_cup[rw] <- grms / conv_table$HEI_WholeFruit_Conv[HEI_index]
    esha_studies$HEI_total_wholeGrains_oz[rw] <- grms / conv_table$HEI_WholeGrains_Conv[HEI_index]
    
    #HEI Conversion for debugging
    esha_studies$HEI_gb_cup_convs[rw] <- conv_table$HEI_GreensBean_Conv[HEI_index]
    esha_studies$HEI_dairy_cup_convs[rw] <- conv_table$HEI_Dairy_Conv[HEI_index]
    esha_studies$HEI_total_pro_oz_convs[rw] <- conv_table$HEI_TotalProtein_Conv[HEI_index]
    esha_studies$HEI_SeaPlantPro_oz_convs[rw] <- conv_table$HEI_SeaPlantPro_Conv[HEI_index]
    esha_studies$HEI_refinedG_oz_convs[rw] <- conv_table$HEI_RefinedGrains_Conv[HEI_index]
    esha_studies$HEI_addedSug_tsp_convs[rw] <- conv_table$HEI_AddedSugar_Conv[HEI_index]
    esha_studies$HEI_total_veg_cup_convs[rw] <- conv_table$HEI_TotalVeg_Conv[HEI_index]
    esha_studies$HEI_total_fruit_cup_convs[rw] <- conv_table$HEI_TotalFruit_Conv[HEI_index]
    esha_studies$HEI_total_wholeFruit_cup_convs[rw] <- conv_table$HEI_WholeFruit_Conv[HEI_index]
    esha_studies$HEI_total_wholeGrains_oz_convs[rw] <- conv_table$HEI_WholeGrains_Conv[HEI_index]
  }
}

# processed_meat <- esha_studies$processed * esha_studies$meat
# esha_studies$processed <- processed_meat

# set missing values to NA, as per Lauren's instruction
esha_studies[is.na(esha_studies)] <- 0

data.table::fwrite(esha_studies, file = file.path(data_dir, "esha_combined_meats_HEI_vals28Jun2025.tsv"),
                   sep = "\t", row.names = F)
openxlsx::write.xlsx(esha_studies, file = file.path(data_dir, "esha_combined_meats_HEI_vals28Jun2025.xlsx"))

new_cols <- c("Intervention",
              "Item_Name",
              "HEI_gb_cup",
              "HEI_dairy_cup",
              "HEI_total_pro_oz",
              "HEI_SeaPlantPro_oz",
              "HEI_refinedG_oz",
              "HEI_addedSug_tsp",
              "HEI_total_veg_cup",
              "HEI_total_fruit_cup",
              "HEI_total_wholeFruit_cup",
              "HEI_total_wholeGrains_oz")

map_hi_unpro <- esha_studies[esha_studies$Intervention == "High HEI Min Processed", new_cols]

unique(esha_studies$Intervention)
test_item <- esha_studies[which(esha_studies$Item_Name == "preserves, strawberry"), new_cols]
# Should be:
# preserves, strawberry: 0.056000448			2.312138728

test_item <- esha_studies[esha_studies$Item_Name == "oats, steel cut, dry, whole grain, gluten free", new_cols]


##### Columns for debugging difference between mine and Patricks work #####
debug_cols <- c("Intervention",
              "Item_Name",
              "Gram_weight",
              "HEI_gb_cup",
              "HEI_dairy_cup",
              "HEI_total_pro_oz",
              "HEI_SeaPlantPro_oz",
              "HEI_refinedG_oz",
              "HEI_addedSug_tsp",
              "HEI_total_veg_cup",
              "HEI_total_fruit_cup",
              "HEI_total_wholeFruit_cup",
              "HEI_total_wholeGrains_oz",
              "HEI_gb_cup_convs",
              "HEI_dairy_cup_convs",
              "HEI_total_pro_oz_convs",
              "HEI_SeaPlantPro_oz_convs",
              "HEI_refinedG_oz_convs",
              "HEI_addedSug_tsp_convs",
              "HEI_total_veg_cup_convs",
              "HEI_total_fruit_cup_convs",
              "HEI_total_wholeFruit_cup_convs",
              "HEI_total_wholeGrains_oz_convs")

debug_df <- esha_studies[,debug_cols]

debug_df <- debug_df[order(debug_df$Intervention),]
openxlsx::write.xlsx(debug_df, file = file.path("output", "unit_test", "tables", "all_map_med_HEI_convers_debug.xlsx"))

for (i in 4:13){#HEI columns stop at 12
  my_col <- debug_cols[i]
  debug_save <- debug_df[,c("Intervention", "Item_Name", "Gram_weight", my_col, paste0(my_col, "_convs"))]
  # debug_save <- debug_save[with(debug_save, order("Intervention", my_col)),]
  write.csv(debug_save,
            file = file.path("output", "unit_test", "tables",
                             paste0(my_col,"_map_med_HEI_convers_debug.csv")),
            row.names = FALSE)
}

##### Finding overlap and difference in MED and MAP items #####
med_studies <- c("MED 0.5oz BEEF", "MED 2.5oz BEEF","MED 5.5oz BEEF",
                 "TYPICAL AMERICAN")

med_items <- unique(unlist(esha_studies[esha_studies$Intervention %in% med_studies, "Item_Name"]))

unique_to_med_items <- setdiff(med_items, unlist(conv_table$item))

full_items <- data.frame(matrix(nrow = length(unique_to_med_items),
                                ncol = ncol(conv_table)))
names(full_items) <- names(conv_table)

full_items$item <- unique_to_med_items

full_items <- rbind(conv_table, full_items)

data.table::fwrite(full_items, file = file.path("data", "diet", "nutrition_data", "MAP_MED_conv_table_wide.tsv"),
                   sep = "\t", row.names = F)

print("Reached end of R script!")




