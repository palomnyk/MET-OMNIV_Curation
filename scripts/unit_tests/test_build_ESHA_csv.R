# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for testing lognorm impmentation
# Test 1 uses the "FormwardReads" DADA2 output from Zeller

####Defining functions -----------------------------------------------------####
print("Defining functions")

#Loading dependencies ------------------------------------------------------
print("Loading dependencies")
# --------------------------------------------------------------------------
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("testthat")) install.packages("testthat")
library("testthat")
if (!requireNamespace("data.table", quietly = TRUE)) BiocManager::install("data.table")
library("data.table")
if (!requireNamespace("readxl", quietly = TRUE))  BiocManager::install("readxl")
library("readxl")
####Establishing directory layout and other constants-----------------------####
print("Establishing directory layout and other constants.")
data_dir <- file.path("..","..", "nutrition_data")
print(getwd())

####Set up constants--------------------------------------------------------####
print("Setting up other constants")
# --------------------------------------------------------------------------
test_columns <- c("Day","Meal","Recipe","Item.Name")

manual_day2 <- data.frame(data.table::fread(file = file.path(data_dir, 
                                                             "MED_training_set.tsv"),
                                            header=TRUE, data.table=FALSE))
manual_day2 <- manual_day2[,test_columns]

manual_day2 <- manual_day2[order(sort(manual_day2$Item.Name)),]

sample_df <- data.frame(data.table::fread(file = file.path(data_dir, 
                                                               "combined_esha_studies.tsv"),
                                              header=TRUE, data.table=FALSE))
#Subset data to make final sample version for testing
final_sample <- sample_df[sample_df$Day == "Day 2" 
                                & sample_df$Study == "MED Diet Complete ESHA Analysis Spreadsheet.xlsx"
                                & sample_df$Intervention == "TYPICAL AMERICAN", 
                                test_columns]
final_sample <- final_sample[order(sort(final_sample$Item.Name)),]

####Testing combined data---------------------------------------------------####
test_that("Testing Day 2 of combined ESH",
          {
           expect_equivalent(final_sample, manual_day2)
          })

print("Completed script!")