# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for shuffling column of dataframe. The main usecase of this is to 
# shuffle the PARENT_SAMEPLE_NAME column of response datasets to create negative
# controls for testing the models.

rm(list = ls()) #clear workspace

print(paste("Working in", getwd()))

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("networkD3", quietly = TRUE))  BiocManager::install("networkD3")
library("networkD3")
if (!requireNamespace("optparse", quietly = TRUE)) BiocManager::install("optparse")
library("optparse")

print("Loaded dependencies")
source(file.path("scripts","data_org", "data_org_func.R"))

#### Functions ####

#### Parse command line arguments ####
option_list <- list(
  optparse::make_option(c("-i", "--in_csv"), type="character", 
                        default=file.path("data", "mapping", "MB_IIT-rf_meats.csv"), 
                        help="input file in csv format"),
  optparse::make_option(c("-o", "--out_file"), type="character",
                        default = file.path("output", "ml_eval"),
                        help="Path of output directory."),
  optparse::make_option(c("-c", "--rand_col"), type="character", 
                        default="PARENT_SAMPLE_NAME",
                        help="Column to shuffle")
);
opt_parser <- optparse::OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

print("Commandline arguments:")
print(opt)

#### Establish directory layout and other constants ####
my_table <- read.csv(opt$in_csv, check.names = FALSE)

print(my_table[,opt$rand_col])

my_table[,opt$rand_col] <- sample(my_table[,opt$rand_col])

print(my_table[,opt$rand_col])

write.csv(my_table, file = opt$out_file, row.names = FALSE)

print("finished running R script")


  