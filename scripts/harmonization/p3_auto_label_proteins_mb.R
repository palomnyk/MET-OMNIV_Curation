#Author: Aaron Yerke (aaronyerke@gmail.com)
#Script for labeling the protein sources based off of rudimentary text mining
#Runs directly off of the output from "p2_add_FPED_data.R
#And adding HEI values
#HEI_conversion_table_LEO wide.xlsx was hand made by lauren for checking our work.
#Need to separate this script into two steps to check out results.

#write script to add MB items to df with same columns as HEI_conversion_table_LEO wide.xlsx and rerun this script with it.

#Note on MB recipes:
# 2 burritos -use 0.2 lb (90 g) for  1 portions (90G person)
# burger -use 1 lb (454 g) for 4 portions (114g per person)
# stew - use 1.5 lb (681 g) for 5 portions (136g per person)
# fajitas use 1.5 lb (681 g) for 6 portions (114g per person)
# stir fry -use 1 lb (454 g) for 4 portions (114g per person)

# beef patty -use 1 lb (454 g) for 4 portions (114g per person)

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
if (!requireNamespace("openxlsx", quietly = TRUE))  BiocManager::install("openxlsx")
library("openxlsx")
if (!requireNamespace("optparse", quietly = TRUE)) BiocManager::install("optparse")
library("optparse")
print("Libraries are loaded.")

### Parse command line arguments ####
option_list <- list(
  optparse::make_option(c("-p", "--HEI_conv_wid"), type="character",
                        # default = "data/diet/nutrition_data/HEI_conversion_table_wide.tsv",
                        default = "data/diet/nutrition_data/mb-HEI_conversion_table_wide.tsv",
                        help="path of wide HEI conversion table"),
  optparse::make_option(c("-c", "--check"), type="logical",
                        default = FALSE,
                        help="Compare results to manual?"),
  optparse::make_option(c("-m", "--manual"), type="character",
                        default = "data/diet/nutrition_data/HEI_conversion_table_LEO wide.xlsx",
                        help="File that lauren manually organized"),
  optparse::make_option(c("-s", "--manual_sheet_num"), type="integer",
                        default = "2",
                        help="File that lauren manually organized"),
  optparse::make_option(c("-o", "--out_prefix"), type="character",
                        # default = "",
                        default = "mb-",
                        help="Prefix to show which group of data this is, since 
                        we didn't add to the first dataset, this is empty.")
);
opt_parser <- optparse::OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

print("Commandline arguments:")
print(opt)

#### Establish directory layout and other constants ####
data_dir <- file.path("data", "diet", "nutrition_data")

HEI_wide <- data.table::fread(file = opt$HEI_conv)
HEI_wide <- HEI_wide[!duplicated(HEI_wide),]

#### Dictionaries for classifying food ####
protein_source_pos <- list(
                           "beef" = c("beef", "steak"),
                           "turkey" = c("turkey"),
                           "lamb" = c("lamb", "mutton", "sheep"),
                           "chicken" = c("chicken","chix"),
                           "pork" = c("pork", "bacon", "fatback", "sausage", "ham ", "ham,", " ham ", "ham"),
                           "seafood" = c("shrimp", "crab", "fish", "lobster", "salmon", "tuna", "cod")
                           #Can ignore mprocessed
                           # "mprocessed" = c("steak", "chops", "ground", "baked", 
                           #                  "broiled", "roast", "fillet", "saute", 
                           #                  "sauté")
                           # "plant-based" = c("nuts", "almond", "bean")
                           # "dairy" = c("yogurt", "yoghurt", "milk", "cheese"),
                           )#strings to select protein source
protein_source_cancel <- list("beef" = c("impossible", "beyond", "broth", "base", "stock", "chicken"),
                              "pork" = c("impossible", "beyond", "base", "stock", "turkey"),
                              "chicken" = c("impossible", "beyond", "broth", "base", "stock", "seasoning"),
                              "turkey" = c("impossible", "beyond", "hill", "broth", "base", "stock"),
                              "lamb" = c("broth", "base", "stock"),
                              "seafood" = c("goldfish")
                              )#strings to cancel protein source selection

processed_indicators <- c("sausage", "sandwich", "bratworst", "smoked",
                "hormel", "bologna", "cured", "deli", "lunchmeat", "hot dog",
                "salted", "pickled", "bacon", "BBQ", "barbeque", "hotdog",
                "patties", "nuggets", "tenders", "jerky", "jerked")


# 2 burritos -use 0.2 lb (90 g) for  1 portions (90G person)
# burger -use 1 lb (454 g) for 4 portions (114g per person)
# stew - use 1.5 lb (681 g) for 5 portions (136g per person)
# fajitas use 1.5 lb (681 g) for 6 portions (114g per person)
# stir fry -use 1 lb (454 g) for 4 portions (114g per person)
# beef patty -use 1 lb (454 g) for 4 portions (114g per person)
mb_item <- c("MB2112, beef burrito",
             "MB2112, beef burger",
             "MB2112, beef stir fry",
             "MB2112, beef stew",
             "MB2112, beef fajitas",
             "MB2112 beef patty",
             "MB2112, chicken burrito",
             "MB2112, chicken fajitas",
             "MB2112, chicken burger",
             "MB2112, chicken stew",
             "MB2112, chicken stir fry")
mb_beef <- c(90, 114, 114, 136, 114, 114, 0, 0, 0, 0, 0)
mb_chicken <- c(0,0,0,0,0,0, 90, 114, 114, 136, 114)

mb_recipes <- data.frame(item = mb_item, beef = mb_beef, chicken = mb_chicken,
                         turkey = rep(0, length(mb_item)),
                         pork = rep(0, length(mb_item)),
                         processed = rep(0, length(mb_item)),
                         meat = mb_beef + mb_chicken)

#df to fill out
pro_source_prediction <- data.frame("item" = character(length = nrow(HEI_wide)), 
                                "beef" = rep(0, nrow(HEI_wide)),
                                "pork" = rep(0, nrow(HEI_wide)),
                                "lamb" = rep(0, nrow(HEI_wide)),
                                "chicken" = rep(0, nrow(HEI_wide)),
                                "turkey" = rep(0, nrow(HEI_wide)),
                                "seafood" = rep(0, nrow(HEI_wide)),
                                "meat" = logical(length = nrow(HEI_wide)),
                                "processed" = logical(length = nrow(HEI_wide)))
#### Cycle through HEI_wide items and label them ####
for (rw in 1:nrow(HEI_wide)){
  itm <- HEI_wide$item[rw]
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
HEI_wide$beef <- pro_source_prediction$beef
HEI_wide$pork <- pro_source_prediction$pork
HEI_wide$chicken <- pro_source_prediction$chicken
HEI_wide$lamb <- pro_source_prediction$lamb
HEI_wide$turkey <- pro_source_prediction$turkey
HEI_wide$seafood <- pro_source_prediction$seafood
HEI_wide$processed <- pro_source_prediction$processed
HEI_wide$red_meat <- pro_source_prediction$beef + pro_source_prediction$pork
HEI_wide$poultry <- pro_source_prediction$chicken + pro_source_prediction$turkey
HEI_wide$meat <- pro_source_prediction$meat

#Min processed meat
HEI_wide$mproc_meat <- pro_source_prediction$meat * !pro_source_prediction$processed
HEI_wide$mproc_beef <- pro_source_prediction$beef * !pro_source_prediction$processed
HEI_wide$mproc_pork <- pro_source_prediction$pork * !pro_source_prediction$processed
HEI_wide$mproc_chicken <- pro_source_prediction$chicken * !pro_source_prediction$processed
HEI_wide$mproc_turkey <- pro_source_prediction$turkey * !pro_source_prediction$processed

#processed meat
HEI_wide$proc_meat <- pro_source_prediction$meat * pro_source_prediction$processed
HEI_wide$proc_beef <- pro_source_prediction$beef * pro_source_prediction$processed
HEI_wide$proc_pork <- pro_source_prediction$pork * pro_source_prediction$processed
HEI_wide$proc_chicken <- pro_source_prediction$chicken * pro_source_prediction$processed
HEI_wide$proc_turkey <- pro_source_prediction$turkey * pro_source_prediction$processed

data.table::fwrite(HEI_wide, file = file.path(data_dir, paste0(opt$out_prefix,"HEI_with_proportions_long.tsv")), 
                   sep = "\t", row.names = F)
openxlsx::write.xlsx(HEI_wide, file = file.path(data_dir, paste0(opt$out_prefix,"HEI_with_proportions_long.xlsx")))

#### Compare meat processing and type results to Lauren's manual labeling ####
if (opt$check){
  my_excel <- readxl::read_excel(opt$manual, sheet = opt$manual_sheet_num, 
                                 trim_ws = F, na = c("", "NA"))
  print("Removing dupiclated rows")
  my_excel <- my_excel[!duplicated(my_excel),]
  
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
}

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
  # Do this in separate script
# 7. Lauren to do after this: spot check HEI and meat amounts to see if conversion factors are okay.


#### Add same information to combined_esha and reuse some of the same code ####
# esha_studies <- readxl::read_excel(file.path(data_dir, "esha_combined_meats_HEI_vals_LEO_noRed.xlsx"),
esha_studies <- data.table::fread(file.path(data_dir, paste0(opt$out_prefix,"combined_esha_studies_HEI.tsv")),
                                  strip.white=FALSE, na.strings = c("", "NA"))
esha_studies <- type.convert(esha_studies, as.is = TRUE)#automatically reset incorrectly classified column types

#df to fill out
diet_items <- unique(esha_studies$`Item Name`)
pro_source_prediction <- data.frame("item" = character(length = length(diet_items)), 
                                    "beef" = rep(0, length(diet_items)),
                                    "lamb" = rep(0, length(diet_items)),
                                    "pork" = rep(0, length(diet_items)),
                                    "chicken" = rep(0, length(diet_items)),
                                    "turkey" = rep(0, length(diet_items)),
                                    "seafood" = rep(0, length(diet_items)),
                                    "meat" = logical(length = length(diet_items)),
                                    "processed" = logical(length = length(diet_items)))

#### Cycle through ESHA_studies items and label them ####
for (rw in 1:length(diet_items)){
  itm <- diet_items[rw]
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

#### Use HEI values to convert g to oz  ####
esha_studies$beef <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$lamb <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$pork <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$chicken <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$turkey <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$seafood <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$processed <- vector(length = nrow(esha_studies), mode = "logical")
# esha_studies$red_meat <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$poultry <- vector(length = nrow(esha_studies), mode = "double")
esha_studies$meat <- vector(length = nrow(esha_studies), mode = "double")

# #Min processed meat
# esha_studies$mproc_meat <- vector(length = nrow(esha_studies), mode = "double")
# esha_studies$mproc_beef <- vector(length = nrow(esha_studies), mode = "double")
# esha_studies$mproc_pork <- vector(length = nrow(esha_studies), mode = "double")
# esha_studies$mproc_chicken <- vector(length = nrow(esha_studies), mode = "double")
# esha_studies$mproc_turkey <- vector(length = nrow(esha_studies), mode = "double")
# 
# #processed meat
# esha_studies$proc_meat <- vector(length = nrow(esha_studies), mode = "double")
# esha_studies$proc_beef <- vector(length = nrow(esha_studies), mode = "double")
# esha_studies$proc_pork <- vector(length = nrow(esha_studies), mode = "double")
# esha_studies$proc_chicken <- vector(length = nrow(esha_studies), mode = "double")
# esha_studies$proc_turkey <- vector(length = nrow(esha_studies), mode = "double")

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
  my_item <- esha_studies$`Item Name`[rw]
  grms <- as.numeric(esha_studies$`Wgt (g)`[rw])
  if (my_item %in% mb_recipes$item){
    esha_studies$beef[rw] <- mb_recipes[mb_recipes$item == my_item, "beef"]
    esha_studies$chicken[rw] <- mb_recipes[mb_recipes$item == my_item, "chicken"]
    esha_studies$meat[rw] <- mb_recipes[mb_recipes$item == my_item, "meat"]
    esha_studies$pork[rw] <- 0
    esha_studies$lamb[rw] <- 0
    
    esha_studies$turkey[rw] <- 0
    esha_studies$seafood[rw] <- 0
    esha_studies$processed[rw] <- 0
    # esha_studies$red_meat[rw] <- grms * as.numeric(pro_source_prediction$red_meat[HEI_index])
    # esha_studies$poultry[rw] <- grms * as.numeric(pro_source_prediction$poultry[HEI_index])
    
  }else{
    if(my_item %in% pro_source_prediction$item){
      HEI_index <- which(pro_source_prediction$item == my_item)
      esha_studies$beef[rw] <- grms * as.numeric(pro_source_prediction$beef[HEI_index])
      esha_studies$pork[rw] <- grms * as.numeric(pro_source_prediction$pork[HEI_index])
      esha_studies$lamb[rw] <- grms * as.numeric(pro_source_prediction$lamb[HEI_index])
      esha_studies$chicken[rw] <- grms * as.numeric(pro_source_prediction$chicken[HEI_index])
      esha_studies$turkey[rw] <- grms * as.numeric(pro_source_prediction$turkey[HEI_index])
      esha_studies$seafood[rw] <- grms * as.numeric(pro_source_prediction$seafood[HEI_index])
      esha_studies$processed[rw] <- pro_source_prediction$processed[HEI_index]
      # esha_studies$red_meat[rw] <- grms * as.numeric(pro_source_prediction$red_meat[HEI_index])
      # esha_studies$poultry[rw] <- grms * as.numeric(pro_source_prediction$poultry[HEI_index])
      esha_studies$meat[rw] <- grms * as.numeric(pro_source_prediction$meat[HEI_index])
      
      # #Min processed meat
      # esha_studies$mproc_meat[rw] <- grms * as.numeric(pro_source_prediction$mproc_meat[HEI_index])
      # esha_studies$mproc_beef[rw] <- grms * as.numeric(pro_source_prediction$mproc_beef[HEI_index])
      # esha_studies$mproc_pork[rw] <- grms * as.numeric(pro_source_prediction$mproc_pork[HEI_index])
      # esha_studies$mproc_chicken[rw] <- grms * as.numeric(pro_source_prediction$mproc_chicken[HEI_index])
      # esha_studies$mproc_turkey[rw] <- grms * as.numeric(pro_source_prediction$mproc_turkey[HEI_index])
      # 
      # #processed meat
      # esha_studies$proc_meat[rw] <- grms * as.numeric(pro_source_prediction$proc_meat[HEI_index])
      # esha_studies$proc_beef[rw] <- grms * as.numeric(pro_source_prediction$proc_beef[HEI_index])
      # esha_studies$proc_pork[rw] <- grms * as.numeric(pro_source_prediction$proc_pork[HEI_index])
      # esha_studies$proc_chicken[rw] <- grms * as.numeric(pro_source_prediction$proc_chicken[HEI_index])
      # esha_studies$proc_turkey[rw] <- grms * as.numeric(pro_source_prediction$proc_turkey[HEI_index])
    
    }
  }
}

processed_meat <- esha_studies$processed * esha_studies$meat
esha_studies$processed <- processed_meat
# set missing values to NA, as per Lauren's instruction
# esha_studies[is.na(esha_studies)] <- 0

data.table::fwrite(esha_studies, file = file.path(data_dir, paste0(opt$out_prefix,"esha_combined_meats_HEI_vals.tsv")),
                   sep = "\t", row.names = F)
openxlsx::write.xlsx(esha_studies, file = file.path(data_dir, paste0(opt$out_prefix,"esha_combined_meats_HEI_vals.xlsx")))

print("Reached end of R script!")
