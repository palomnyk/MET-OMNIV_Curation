#Author: Aaron Yerke (aaronyerke@gmail.com)
#Script for adding HEI/FPED categories to ESHA output

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

#### Reading in data and establish constants ####
data_dir <- file.path("nutrition_data")
my_excel <- file.path(data_dir, "HEI Pivot table_modified.xlsx")#modified table 
# has several columns removed that were extra in 2 of the sheets
my_sheets <- readxl::excel_sheets(my_excel)
df <- data.table::fread(file = file.path(data_dir, "combined_esha_studies.tsv"),
                        check.names = F)
req_columns <- c("Category", "1 cup equiv. (g)",	"Total Cups",	"1 oz equiv. (g)",	
                 "Total Ounces",	"1 tsp equiv. (g)",	"Total tsp")


#### Parse pivot sheets ####
big_sheet <- "empty"
for (sht in 1:length(my_sheets)){
  my_sheet <- my_sheets[sht]
  if (!startsWith(my_sheet, "PIVOT")){
    print(my_sheet)
    pivot <- readxl::read_excel(my_excel, sheet = my_sheet)
    if (typeof(big_sheet) != "list") {big_sheet <- pivot}
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

#### Extract data from ####
new_info <- data.frame(matrix(nrow = nrow(df), ncol = length(req_columns)))
colnames(new_info) <- req_columns


for (rw in 1:nrow(df)){
  my_item <- unlist(df[rw,"Item.Name"])
  my_quantity <- unlist(df[rw, "Quantity"])
  my_sub <- subset(big_sheet, `Menu Item` == my_item & `Quantity (g)` == my_quantity)
  my_row <- my_sub[1, req_columns]
  new_info[rw,req_columns] <- my_row
} 

out_df <- cbind(df, new_info)

data.table::fwrite(out_df, file = file.path(data_dir, "combined_esha_studies_HEI.tsv"), 
                   sep = "\t", row.names = F)

out_df <- cbind(out_df[,1:10], new_info)
data.table::fwrite(out_df, file = file.path(data_dir, "combined_esha_studies_HEI_brief.tsv"), 
                   sep = "\t", row.names = F)
# for(i in 1:length(names(big_sheet))){
#   print(paste(i, names(big_sheet)[i], names(pivot)[i]))
#   print(identical(names(big_sheet)[i], names(pivot)[i]))
# }




