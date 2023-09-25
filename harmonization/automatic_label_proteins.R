#Author: Aaron Yerke (aaronyerke@gmail.com)
#Script for labeling the protein sources based off of rudimentary text mining

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
print("Libraries are loaded.")

#Read in data
data_dir <- file.path("nutition_data")
labeled_file <- "HEI_conversion_table_LEO wide.xlsx"
labeled_sheet <- 2
my_excel <- readxl::read_excel(file.path(data_dir, labeled_file), sheet = labeled_sheet, 
                               trim_ws = F)

#### Dictionaries for classifying food ####
protein_source_pos <- list(
                           "beef" = c("beef"),
                           "turkey" = c("turkey"),
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
# Beef  Pork Chicken Turkey Mproc_Beef Mproc_Chicken Mproc_Pork Mproc_Turkey Proc_Beef Proc_Chicken
# <dbl> <dbl>   <dbl>  <dbl>      <dbl>         <dbl>      <dbl>        <dbl>     <dbl>        <dbl>
#   1     1     1       0      0          0             0          0            0         1            0
# # ℹ 2 more variables: Proc_pork <dbl>, Proc_Turkey <dbl>
# # A tibble: 1 × 12
# beef  pork chicken turkey mproc_beef mproc_chicken mproc_pork mproc_turkey proc_beef proc_chicken
# <dbl> <dbl>   <dbl>  <dbl>      <dbl>         <dbl>      <dbl>        <dbl>     <dbl>        <dbl>
#   1   0.5   0.5       0      0          0             0          0            0       0.5            0
# # ℹ 2 more variables: proc_pork <dbl>, proc_turkey <dbl>
# [1] "13 hand first"
# [1] "Bologna- Beef and Pork (BERKS)"
# # A tibble: 1 × 12
# Beef  Pork Chicken Turkey Mproc_Beef Mproc_Chicken Mproc_Pork Mproc_Turkey Proc_Beef Proc_Chicken
# <dbl> <dbl>   <dbl>  <dbl>      <dbl>         <dbl>      <dbl>        <dbl>     <dbl>        <dbl>
#   1     1     1       0      0          0             0          0            0         1            0
# # ℹ 2 more variables: Proc_pork <dbl>, Proc_Turkey <dbl>
# # A tibble: 1 × 12
# beef  pork chicken turkey mproc_beef mproc_chicken mproc_pork mproc_turkey proc_beef proc_chicken
# <dbl> <dbl>   <dbl>  <dbl>      <dbl>         <dbl>      <dbl>        <dbl>     <dbl>        <dbl>
#   1   0.5   0.5       0      0          0             0          0            0       0.5            0
# # ℹ 2 more variables: proc_pork <dbl>, proc_turkey <dbl>
# [1] "78 hand first"
# [1] "turkey, ground, raw, 15% fat"
# # A tibble: 1 × 12
# Beef  Pork Chicken Turkey Mproc_Beef Mproc_Chicken Mproc_Pork Mproc_Turkey Proc_Beef Proc_Chicken
# <dbl> <dbl>   <dbl>  <dbl>      <dbl>         <dbl>      <dbl>        <dbl>     <dbl>        <dbl>
#   1     0     0       0      1          0             0          0            0         0            0
# # ℹ 2 more variables: Proc_pork <dbl>, Proc_Turkey <dbl>
# # A tibble: 1 × 12
# beef  pork chicken turkey mproc_beef mproc_chicken mproc_pork mproc_turkey proc_beef proc_chicken
# <dbl> <dbl>   <dbl>  <dbl>      <dbl>         <dbl>      <dbl>        <dbl>     <dbl>        <dbl>
#   1     0     0       0      1          0             0          0            1         0            0
# # ℹ 2 more variables: proc_pork <dbl>, proc_turkey <dbl>

data.table::fwrite(my_excel, file = file.path(data_dir, "HEI_with_proportions.tsv"), 
                   sep = "\t", row.names = F)


#### Use HEI values to convert g to oz  ####
esha_studies <- data.table::fread(file = file.path(data_dir, "combined_esha_studies.tsv"))

HEI_protein_oz <- vector(length = nrow(esha_studies), mode = "double")

for (rw in 1:nrow(esha_studies)){
  my_item <- esha_studies$Item.Name[rw]
  my_oz <- 0
  if (my_item %in% HEI_only_meat$item){
    HEI_index <- which(HEI_only_meat$item == my_item)
    my_oz <- as.numeric(esha_studies$Quantity[rw]) / as.numeric(HEI_only_meat$HEI_TotalProtein_Conv[HEI_index])
  }
  HEI_protein_oz[rw] <- my_oz
}

esha_studies$meat_HEI_oz <- HEI_protein_oz


data.table::fwrite(esha_studies, file = file.path(data_dir, "esha_combined_HEI_proteins.tsv"), 
                   sep = "\t", row.names = F)

# TODO: Add red meat HEI, poultry, min_processed, and any other variables from table columns
# TODO: IMPROVE COMMENTS
# TODO: ADD FRUITS, VEG, etc from other HEI groups





