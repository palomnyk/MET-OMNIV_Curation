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
                        default = "data/mapping/all_sites_meats.csv",
                        # default = "data/mapping/Purdue-meats_g.csv",
                        help="path of first csv"),
  optparse::make_option(c("-s", "--secnd_path"), type="character",
                        # default = "Data/demo/helper_features_2009-2020.csv",
                        default = "data/mapping/all_sites_demo.csv",
                        help="Path of second csv."),
  optparse::make_option(c("-o", "--out_path"), type="character",
                        default = "data/mapping/Purdue-meats_g-demo.csv",
                        help="Path of output csv."),
  optparse::make_option(c("-i", "--id_var"), type="character",
                        default="PARENT_SAMPLE_NAME",
                        help="Path of second csv."),
  optparse::make_option(c("-k", "--keep"), type="character",
                        default="all",
                        help="Options in merge function to keep all of both dfs (all) or all of first df (all.x)")
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
print(paste("1st df nrow:", nrow(first_df)))
secnd_df <- read.csv(opt$secnd_path, header = T, check.names = F,
                     row.names=opt$id_var)
print(paste("2nd df nrow:", nrow(secnd_df)))
print("Data loaded!")

if (opt$keep == "all"){
  final_table <- merge(first_df, secnd_df, all = TRUE, by = 0)
}
if (opt$keep == "all.x"){
  final_table <- merge(first_df, secnd_df, all.x = TRUE, by = 0)
}
print(paste("final df nrow:", nrow(final_table)))

#### Designate selected column as row names ####
final_table[,opt$id_var] <- final_table[,"Row.names"]
# Drop it after making it into row names
final_table <- final_table[,!names(final_table) %in% c("Row.names")]

write.csv(final_table, file = opt$out_path, row.names = FALSE)

print("Reached end of R script!")

