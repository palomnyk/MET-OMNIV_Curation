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
req_columns <- c("Category","","1 oz equiv. (g)", "1 tsp equiv. (g)")
conversion_cols <- c("1 cup equiv. (g)","1 oz equiv. (g)", "1 tsp equiv. (g)")

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

#### Extract data from HEI table####
# new_info <- data.frame(matrix(nrow = nrow(df), ncol = length(req_columns)))
# colnames(new_info) <- req_columns

conv_item <- c()
conv_cat <- c()
conv_equ <- c()
conv_unit <- c()

for (rw in 1:nrow(big_sheet)){
  my_cat <- unlist(big_sheet[rw,"Category"])
  my_convers <- which(!is.na(unlist(big_sheet[rw, conversion_cols])))
  if (!all(is.na(my_convers))){
    my_item <- unlist(big_sheet[rw,"Menu Item"])
    if (!my_item %in% conv_item){
      my_quant <- unlist((big_sheet[rw, "Quantity (g)"]))
      #pick the value that is not NA
      my_convers <- which(!is.na(unlist(big_sheet[rw, conversion_cols])))
      
      my_unit <- names(my_convers)[1]
      conv_cat <- c(conv_cat, my_cat)
      conv_item <- c(conv_item, my_item)
      conv_equ <- c(conv_equ, unlist(big_sheet[rw, my_unit]))
      conv_unit <- c(conv_unit, my_unit)
    }
  }
}

conv_tab_names <- c("item","Category","HEI_equiv","HEI_unit") 
conversion_table <- data.frame(conv_item, conv_cat, conv_equ, conv_unit)
names(conversion_table) <-  conv_tab_names
data.table::fwrite(conversion_table, file = file.path(data_dir, "HEI_conversion_table.tsv"),
          sep = "\t", row.names = F)

#### Add HEI data to ESHA combined dataset ####
new_info <- data.frame(matrix(nrow = nrow(df), ncol = length(req_columns)))
colnames(new_info) <- conv_tab_names
names(new_info)[1] <- "HEI_volumne"

for (rw in 1:nrow(df)){
  my_item <- unlist(df[rw,"Item.Name"])
  if (my_item %in% conversion_table$item){
    my_quantity <- unlist(df[rw, "Quantity"])
    my_convers_row <- conversion_table[which(conversion_table$item == my_item),]
    conv_item <- my_convers_row[1]
    if (my_item == conv_item){
      my_unit <- my_convers_row[4]
      my_HEI_num <- my_convers_row[3]
      my_cat <- my_convers_row[2]
      my_value <- my_quantity/my_HEI_num
      # print(paste(my_item, my_convers_row))
      # my_row <- my_sub[1, req_columns]
      new_info[rw,] <- c(my_value, my_cat, my_HEI_num, my_unit)
    }
  }
} 

out_df <- cbind(df, new_info)

data.table::fwrite(out_df, file = file.path(data_dir, "combined_esha_studies_HEI.tsv"), 
                   sep = "\t", row.names = F)

missing_HEI <- out_df[is.na(out_df$HEI_volumne), ]
missing_items <- unique(missing_HEI$Item.Name)

data.table::fwrite(list(missing_items), file = file.path(data_dir, "HEI_missing_items.tsv"),
                   sep = "\t", row.names = F)





