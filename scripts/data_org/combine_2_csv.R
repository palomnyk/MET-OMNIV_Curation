# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for combining two predictor variables along a specific column.

rm(list = ls()) #clear workspace
chooseCRANmirror(graphics=FALSE, ind=66)

### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("optparse", quietly = TRUE)) BiocManager::install("optparse")
library("optparse")

print("Loaded dependencies")
source(file.path("scripts","data_org", "data_org_func.R"))

#### Functions ####

#### Parse command line arguments ####
option_list <- list(
  optparse::make_option(c("-f", "--first_path"), type="character",
                        # default = "Data/diet/d1_nutri_food_2015.csv",
                        default = "data/mapping/all_sites_meats.csv",
                        help="path of first csv"),
  optparse::make_option(c("-s", "--secnd_path"), type="character",
                        # default = "Data/demo/helper_features_2009-2020.csv",
                        default = "data/mapping/all_sites_demo.csv",
                        help="Path of second csv."),
  optparse::make_option(c("-o", "--out_path"), type="character",
                        default = "data/mapping/all_sites_metadata_auto-pro_demo.csv",
                        help="Path of output csv."),
  optparse::make_option(c("-i", "--id_var"), type="character",
                        default="PARENT_SAMPLE_NAME",
                        help="Path of second csv.")
);
opt_parser <- optparse::OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

print("Commandline arguments:")
print(opt)

#### Establish directory layout and other constants ####
output_dir <- file.path(dirname(opt$out_path))
dir.create(output_dir)

#### Loading in data ####
first_df <- read.csv(opt$first_path, header = T, check.names = F,
                     row.names=opt$id_var)
secnd_df <- read.csv(opt$secnd_path, header = T, check.names = F,
                     row.names=opt$id_var)
print("Data loaded!")

final_table <- merge(first_df, secnd_df, all = TRUE, by = 0)

# row.names(final_table) <- final_table[,"Row.names"]

final_table[,opt$id_var] <- final_table[,"Row.names"]
final_table <- final_table[,!names(final_table) %in% c("Row.names")]

write.csv(final_table, file = opt$out_path, row.names = FALSE)

print("Reached end of R script!")

