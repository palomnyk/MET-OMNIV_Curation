#Author: Aaron Yerke (aaronyerke@gmail.com)
#Script for creating HEI/FPED categories conversion table from Pat's Pivot tables.
# Inputs: 
#   1. HEI pivot table with dietary items and HEI equivalence categories and
#     conversion factors.
#   2. Table with esha dietary information with new categories
# Outputs:
#   1. Long format HEI conversion table
#   2. Wide format HEI conversion table

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
if (!requireNamespace("tools", quietly = TRUE))  BiocManager::install("tools")
library("tools")
if (!requireNamespace("optparse", quietly = TRUE)) BiocManager::install("optparse")
library("optparse")
print("Libraries are loaded.")

#### Functions ####
title_case <- function(y) {
  my_strings <- strsplit(y, " ")[[1]]
  paste(toupper(substring(my_strings, 1,1)), substring(my_strings, 2),
        sep="", collapse=" ")
}

#### Parse command line arguments ####
option_list <- list(
  optparse::make_option(c("-p", "--HEI_conv"), type="character",
                        default = "data/diet/nutrition_data/HEI Pivot table.xlsx",
                        help="path of HEI conversion table"),
  optparse::make_option(c("-i", "--input"), type="character",
                        default = "data/diet/nutrition_data/combined_esha_studies.tsv",
                        # default = "data/diet/nutrition_data/mb_2112_esha.tsv",
                        help='Dietary data with the following columns: "Study"             "Intervention"      "Day"               "Meal"             
 "Recipe"            "Item Name"         "Quantity"          "Measure"          
  "Wgt (g)"           "Cals (kcal)", and other nutritional features'),
  optparse::make_option(c("-o", "--out_prefix"), type="character",
                        default = "",
                        # default = "mb-",
                        help="Prefix to show which group of data this is, since 
                        we didn't add to the first dataset, this is empty.")
);
opt_parser <- optparse::OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

print("Commandline arguments:")
print(opt)

#### Establish directory layout and other constants ####

#### Reading in data and establish constants ####
data_dir <- file.path("data", "diet", "nutrition_data")
my_excel <- file.path(opt$HEI_conv)#default is modified table 
# has several columns removed that were extra in 2 of the sheets
my_sheets <- readxl::excel_sheets(my_excel)
combined_studies <- data.table::fread(file = file.path(opt$input),
                        check.names = FALSE)
req_columns <- c("Category","","1 oz equiv. (g)", "1 tsp equiv. (g)")
conversion_cols <- c("1 cup equiv. (g)","1 oz equiv. (g)", "1 tsp equiv. (g)")

#### Parse pivot sheets ####
big_sheet <- "empty"
for (sht in 1:length(my_sheets)){
  my_sheet <- my_sheets[sht]
  if (!startsWith(my_sheet, "PIVOT")){
    print(my_sheet)
    pivot <- readxl::read_excel(my_excel, sheet = my_sheet)
    if (typeof(big_sheet) != "list") big_sheet <- pivot
    else{
      print(length(names(big_sheet)))
      print(length(names(pivot)))
      if (identical(names(big_sheet), names(pivot))){
        print(paste("Adding", my_sheet, "to big table"))
        big_sheet <- rbind(big_sheet,pivot)
      }
    }
  }
}

big_sheet <- big_sheet[!is.na(big_sheet$Meal),]

#### Extract data from HEI table####
conv_item <- c()
conv_cat <- c()
conv_equ <- c()
conv_unit <- c()

need_cup <- c("Dairy", "Greens and Beans", "Total Vegetable", "Total Fruit",
              "Whole Grain","Whole Fruit")
need_oz <- c("Seafood and Plant Protein","Refined Grain", "Total Protein")

for (rw in 1:nrow(big_sheet)){
  my_convers <- which(!is.na(unlist(big_sheet[rw, conversion_cols])))
  if (!all(is.na(my_convers))){#pick the value that is not NA
    my_item <- unlist(big_sheet[rw,"Menu Item"])
    my_cats <- unlist(strsplit(unlist(big_sheet[rw,"Category"]), "/ "))
    for (my_cat in my_cats){
      if (!is.na(my_cat)){
        my_cat <- tools::toTitleCase(trimws(unlist(my_cat)))
        if (my_cat %in% need_cup){
          my_unit <- "1 cup equiv. (g)"
          conv_cat <- c(conv_cat, my_cat)
          conv_item <- c(conv_item, my_item)
          conv_equ <- c(conv_equ, unlist(big_sheet[rw, my_unit]))
          conv_unit <- c(conv_unit, my_unit)
        }else{
          if (my_cat %in% need_oz){
            my_unit <- "1 oz equiv. (g)"
            conv_cat <- c(conv_cat, my_cat)
            conv_item <- c(conv_item, my_item)
            conv_equ <- c(conv_equ, unlist(big_sheet[rw, my_unit]))
            conv_unit <- c(conv_unit, my_unit)
          }else{
            if (my_cat == "Added Sugar"){
              my_unit <- "1 tsp equiv. (g)"
              conv_cat <- c(conv_cat, my_cat)
              conv_item <- c(conv_item, my_item)
              conv_equ <- c(conv_equ, unlist(big_sheet[rw, my_unit]))
              conv_unit <- c(conv_unit, my_unit)
            }else{
              stop(paste("HEI category not found:", my_cat))
            }
          }
        }
      }
    }
  }
}

#### Create long HEI conversion table####
conv_tab_names <- c("item","Category","HEI_equiv","HEI_unit") 
conversion_table <- data.frame(conv_item, conv_cat, conv_equ, conv_unit)

names(conversion_table) <- conv_tab_names

data.table::fwrite(conversion_table, file = file.path(data_dir, paste0(opt$out_prefix, "HEI_conversion_table_long.tsv")),
                   sep = "\t", row.names = F)

#### Create wide HEI conversion table ####
wide_columns <- c("item", "HEI_TotalVeg_Cat", "HEI_TotalVeg_Conv", "HEI_TotalVeg_Unit",
                  "HEI_WholeFruit_Cat", "HEI_WholeFruit_Conv", "HEI_WholeFruit_Unit",
                  "HEI_WholeFruit_Cat", "HEI_WholeFruit_Conv", "HEI_WholeFruit_Unit",
                  "HEI_TotalFruit_Cat", "HEI_TotalFruit_Conv", "HEI_TotalFruit_Unit",
                  "HEI_GreensBeans_Cat", "HEI_GreensBeans_Conv", "HEI_GreensBeans_Unit",
                  "HEI_WholeGrains_Cat", "HEI_WholeGrains_Conv", "HEI_WholeGrains_Unit",
                  "HEI_Dairy_Cat", "HEI_Dairy_Conv", "HEI_Dairy_Unit", "HEI_TotalProtein_Cat",
                  "HEI_TotalProtein_Conv", "HEI_TotalProtein_Unit", "HEI_SeaPlantPro_Cat",
                  "HEI_SeaPlantPro_Conv", "HEI_SeaPlantPro_Unit", "HEI_RefinedGrains_Cat",
                  "HEI_RefinedGrains_Conv", "HEI_RefinedGrains_Unit", "HEI_AddedSugar_Cat",
                  "HEI_AddedSugar_Conv", "HEI_AddedSugar_Unit")

wide_conv_tabl <- data.frame(matrix(ncol = length(wide_columns)))
names(wide_conv_tabl) <- wide_columns

uniq_items <- unique(combined_studies$`Item Name`)
for (i in 1:length(uniq_items)){
  print(conversion_table$item[i])
  my_row <- rep(NA, length(wide_conv_tabl))
  my_row[1] <- conversion_table$item[i]
  my_HEI_cats <- unlist(strsplit(conversion_table$Category[i], "/ "))
  for(h in my_HEI_cats){
    if(is.na(h)){
      wide_conv_tabl[i,] <- my_row
    }else{
      if (h == "Whole Grain"){
        my_row[match("HEI_WholeGrains_Conv", wide_columns)] <- conversion_table$HEI_equiv[i]
        my_row[match("HEI_WholeGrains_Unit", wide_columns)] <- conversion_table$HEI_unit[i]
        my_row[match("HEI_WholeGrains_Cat", wide_columns)] <- conversion_table$Category[i]
      }
      if (h == "Dairy"){
        my_row[match("HEI_Dairy_Conv", wide_columns)] <- conversion_table$HEI_equiv[i]
        my_row[match("HEI_Dairy_Unit", wide_columns)] <- conversion_table$HEI_unit[i]
        my_row[match("HEI_Dairy_Cat", wide_columns)] <- conversion_table$Category[i]
      }
      if (h == "Added Sugar"){
        my_row[match("HEI_AddedSugar_Conv", wide_columns)] <- conversion_table$HEI_equiv[i]
        my_row[match("HEI_AddedSugar_Unit", wide_columns)] <- conversion_table$HEI_unit[i]
        my_row[match("HEI_AddedSugar_Cat", wide_columns)] <- conversion_table$Category[i]
      }
      if (h == "Total Fruit"){
        my_row[match("HEI_TotalFruit_Conv", wide_columns)] <- conversion_table$HEI_equiv[i]
        my_row[match("HEI_TotalFruit_Unit", wide_columns)] <- conversion_table$HEI_unit[i]
        my_row[match("HEI_TotalFruit_Cat", wide_columns)] <- conversion_table$Category[i]
      }
      if (h == "Refined Grain"){
        my_row[match("HEI_RefinedGrain_Conv", wide_columns)] <- conversion_table$HEI_equiv[i]
        my_row[match("HEI_RefinedGrain_Unit", wide_columns)] <- conversion_table$HEI_unit[i]
        my_row[match("HEI_RefinedGrain_Cat", wide_columns)] <- conversion_table$Category[i]
      }
      if (h == "Total Protein"){
        my_row[match("HEI_TotalProtein_Conv", wide_columns)] <- conversion_table$HEI_equiv[i]
        my_row[match("HEI_TotalProtein_Unit", wide_columns)] <- conversion_table$HEI_unit[i]
        my_row[match("HEI_TotalProtein_Cat", wide_columns)] <- conversion_table$Category[i]
      }
      if (h == "Greens and Beans"){
        my_row[match("HEI_GreensBean_Conv", wide_columns)] <- conversion_table$HEI_equiv[i]
        my_row[match("HEI_GreensBean_Unit", wide_columns)] <- conversion_table$HEI_unit[i]
        my_row[match("HEI_GreensBean_Cat", wide_columns)] <- conversion_table$Category[i]
      }
      if (h == "Total Vegetable"){
        my_row[match("HEI_TotalVegetable_Conv", wide_columns)] <- conversion_table$HEI_equiv[i]
        my_row[match("HEI_TotalVegetable_Unit", wide_columns)] <- conversion_table$HEI_unit[i]
        my_row[match("HEI_TotalVegetable_Cat", wide_columns)] <- conversion_table$Category[i]
      }
      if (h == "Seafood and Plant Protein"){
        my_row[match("HEI_SeaPlantPro_Conv", wide_columns)] <- conversion_table$HEI_equiv[i]
        my_row[match("HEI_SeaPlantPro_Unit", wide_columns)] <- conversion_table$HEI_unit[i]
        my_row[match("HEI_SeaPlantPro_Cat", wide_columns)] <- conversion_table$Category[i]
      }
      if (h == "Whole Fruit"){
        my_row[match("HEI_WholeFruit_Conv", wide_columns)] <- conversion_table$HEI_equiv[i]
        my_row[match("HEI_WholeFruit_Unit", wide_columns)] <- conversion_table$HEI_unit[i]
        my_row[match("HEI_WholeFruit_Cat", wide_columns)] <- conversion_table$Category[i]
      }
    }
    wide_conv_tabl[i,] <- my_row
  }
}
data.table::fwrite(wide_conv_tabl, file = file.path(data_dir, paste0(opt$out_prefix, "HEI_conversion_table_wide.tsv")),
                   sep = "\t", row.names = F)

print("Completed R script!")

