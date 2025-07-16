#Author: Aaron Yerke (aaronyerke@gmail.com)
#Script for labeling the protein sources based off of hand labeled FPED conv table
#Runs directly off of the output from "p2_add_FPED_data.R

# below is the code to create table to add fped data to
# uniq_cols <- c("beef", "lamb", "pork",
#                "chicken", "turkey", "seafood", "processed", "poultry", "meat")
# unique_esha <- esha_studies[, ..uniq_cols]
# unique_esha[unique_esha > 0] <- 1
# unique_esha <- cbind(esha_studies[,c("Item Name", "HEI_equiv", "HEI_unit")], 
#                      unique_esha)
# unique_esha <- unique_esha[!duplicated(unique_esha), ]
# write.csv(unique_esha, file=file.path(data_dir, "mb-unique_esha_fped.csv"))
# This table was then filled in by hand by looking up the food item in FPED-17

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
  optparse::make_option(c("-p", "--FPED_conv"), type="character",
                        default = "data/diet/mb/mb-unique_esha_fped_RR_3-11-25_corrected.tsv",
                        help="path of mb specific FPED conversion table"),
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

FPED_conv <- data.table::fread(file = opt$FPED_conv)

# Clean up the quotes and backslashes in the Item Name column
FPED_conv$`Item Name` <- gsub('"', '', as.character(FPED_conv$`Item Name`))
FPED_conv$`Item Name` <- gsub('\\\\', "", as.character(FPED_conv$`Item Name`))
# FPED_conv <- gsub('\\\\', "", FPED_conv)
FPED_conv <- FPED_conv[!duplicated(FPED_conv),]

esha_studies <- data.table::fread(file.path(data_dir, paste0(opt$out_prefix,"combined_esha_studies_HEI.tsv")),
                                  strip.white=FALSE, na.strings = c("", "NA"))
esha_studies <- type.convert(esha_studies, as.is = TRUE)#automatically reset incorrectly classified column types

# Organize meat type of column names
base_col_names <- c("beef", "chicken", "pork", "turkey", "meat")
agg_columns <- unlist(lapply(base_col_names, function(x){
  c(x, paste0(x, "_proc"), paste0(x, "_min_proc"))
}))

#### Cycle through ESHA_studies items and label them ####

#### Use HEI values to convert g to oz  ####
for (ag in agg_columns){
  esha_studies[,ag] <- vector(length = nrow(esha_studies), mode = "double")
}

for (rw in 1:nrow(esha_studies)){
  my_item <- gsub('"', '', esha_studies$`Item Name`[rw])
  my_item <- gsub('\\\\', "", my_item)
  grms <- as.numeric(esha_studies$`Wgt (g)`[rw])
  FPED_rw <- which(FPED_conv$`Item Name` == my_item)
  processed <- FPED_conv[FPED_rw, "processed"]
  g_meat_per_100g_item <- unlist(FPED_conv[FPED_rw,"oz-g_equiv"])[1]*28/100
  g_meat_per_100g_item <- ifelse(is.na(g_meat_per_100g_item), 0, g_meat_per_100g_item)
  print("g_meat_per_100g_item")
  print(g_meat_per_100g_item)
  print(my_item)
  grms <- grms * g_meat_per_100g_item
  esha_studies$beef[rw] <- grms * FPED_conv[FPED_rw,"beef"] * g_meat_per_100g_item
  esha_studies$beef_min_proc[rw] <- ifelse(processed == 0, grms * FPED_conv[FPED_rw,"beef"] * g_meat_per_100g_item, 0)
  esha_studies$beef_proc[rw] <- ifelse(processed == 1, grms * FPED_conv[FPED_rw,"beef"] * g_meat_per_100g_item, 0)
  
  esha_studies$pork[rw] <- grms * FPED_conv[FPED_rw,"pork"] * g_meat_per_100g_item
  esha_studies$pork_min_proc[rw] <- ifelse(processed == 0, grms * FPED_conv[FPED_rw,"pork"] * g_meat_per_100g_item, 0)
  esha_studies$pork_proc[rw] <- ifelse(processed == 1, grms * FPED_conv[FPED_rw,"pork"] * g_meat_per_100g_item, 0)
  
  esha_studies$chicken[rw] <- grms * FPED_conv[FPED_rw,"chicken"] * g_meat_per_100g_item
  esha_studies$chicken_min_proc[rw] <- ifelse(processed == 0, grms * FPED_conv[FPED_rw,"chicken"] * g_meat_per_100g_item, 0)
  esha_studies$chicken_proc[rw] <- ifelse(processed == 1, grms * FPED_conv[FPED_rw,"chicken"] * g_meat_per_100g_item, 0)
  
  esha_studies$turkey[rw] <- grms * FPED_conv[FPED_rw,"turkey"] * g_meat_per_100g_item
  esha_studies$turkey_min_proc[rw] <- ifelse(processed == 0, grms * FPED_conv[FPED_rw,"turkey"] * g_meat_per_100g_item, 0)
  esha_studies$turkey_proc[rw] <- ifelse(processed == 1, grms * FPED_conv[FPED_rw,"turkey"] * g_meat_per_100g_item, 0)
}

print(esha_studies)
esha_studies$meat <- as.numeric(esha_studies$beef) + as.numeric(esha_studies$pork) + as.numeric(esha_studies$chicken) + as.numeric(esha_studies$turkey)
esha_studies$meat_min_proc <- as.numeric(esha_studies$beef_min_proc) + as.numeric(esha_studies$pork_min_proc) + as.numeric(esha_studies$chicken_min_proc) + as.numeric(esha_studies$turkey_min_proc)
esha_studies$meat_proc <- as.numeric(esha_studies$beef_proc) + as.numeric(esha_studies$pork_proc) + as.numeric(esha_studies$chicken_proc) + as.numeric(esha_studies$turkey_proc)

data.table::fwrite(esha_studies, file = file.path(data_dir, paste0(opt$out_prefix,"esha_meats_FPED_vals.tsv")),
                   sep = "\t", row.names = F)
openxlsx::write.xlsx(esha_studies, file = file.path(data_dir, paste0(opt$out_prefix,"esha_meats_FPED_vals.xlsx")))

print("Reached end of R script!")

