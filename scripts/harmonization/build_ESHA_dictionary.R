#Author: Aaron Yerke (aaronyerke@gmail.com)
#Script for building dictionary from ESHA output
##Meant to give a starting point to our harmonization dictionaries

#TODO - add missing data analysis

#### Loading libraries and data ####
#read in libraries
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("ggplot2", quietly = TRUE))  BiocManager::install("ggplot2")
library("ggplot2")
if (!requireNamespace("readxl", quietly = TRUE))  BiocManager::install("readxl")
library("readxl")
print("Libraries are loaded.")

#Read in data
data_dir <- file.path("nutrition_data")
my_excel <- "MAP Study Complete ESHA Analysis Spreadsheet.xlsx"
my_sheets <- readxl::excel_sheets(file.path(data_dir, my_excel))
my_excel <- readxl::read_excel(file.path(data_dir, my_excel), sheet = my_sheets[1])
my_names <- as.vector(my_excel[2,])
df <- data.frame(my_excel[2:nrow(my_excel),], header = FALSE, check.names = FALSE)
names(df) <- df[1,]
print("Data has been read in.")


#### Parsing input ####
esha <- names(df)
ncba <- sapply(names(df), function(nm){
  print(nm)
  nm <- gsub(" ", "_", nm)
  nm <- gsub("\\(", ".", nm, perl = F, fixed = "F")
  nm <- gsub(")", ".", nm)
  nm <- gsub(":", "-", nm)
  nm <- tolower(nm)
  print(nm)
  return(nm)
})

#### Build and save dictionary ####
out_df <- data.frame(esha = esha, ncba = ncba)
write.csv(out_df,sep="\t",file = "dictionary.tsv")


