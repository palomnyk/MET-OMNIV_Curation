#Author: Aaron Yerke (aaronyerke@gmail.com)
#Script for building CSV from ESHA output
##Meant to give a starting point to our harmonization dictionaries

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
excel_files <- c("MAP Study Complete ESHA Analysis Spreadsheet.xlsx", 
                 "MED Diet Complete ESHA Analysis Spreadsheet.xlsx")

#### Check if all files have the same column headers ####
last_names <- "empty"# for holding headers of previous excel sheet during check
for (ef in 1:length(excel_files)){
  my_file <- excel_files[ef]
  my_sheets <- readxl::excel_sheets(file.path(data_dir, my_file))
  for (sht in 1:length(my_sheets)){
    my_sheet <- my_sheets[sht]
    my_excel <- readxl::read_excel(file.path(data_dir, my_file), sheet = my_sheet, 
                                   trim_ws = F)
    my_names <- as.vector(my_excel[2,])
    if (typeof(last_names) == "character"){
      last_names <- my_names
    }else{
      if (identical(my_names,last_names)){
        print(paste(my_file, my_sheet, "not the same as last"))
        stop()
      }else{
        print(paste(my_file, my_sheet))
      }
    }
  }#for sht
}#for ef
#If this does not stop at stop(), assume that all sheets have the same column names

#### Create output dataframe ####
out_names <- c("Study", "Intervention", "Day", "Meal", "Recipe", last_names)
out_df <- data.frame(matrix(1:length(out_names), ncol = length(out_names)))
names(out_df) <- out_names
rows_added <- 1

#### Cycle through excel files and sheets ####
for (ef in 1:length(excel_files)){
  my_file <- excel_files[ef]
  my_sheets <- readxl::excel_sheets(file.path(data_dir, my_file))
  for (sht in 1:length(my_sheets)){
    my_sheet <- my_sheets[sht]
    my_excel <- readxl::read_excel(file.path(data_dir, my_file), sheet = my_sheet, 
                                   trim_ws = F)
    my_names <- as.vector(my_excel[2,])
    df <- my_excel[3:nrow(my_excel),]
    # df <- data.frame(my_excel[3:nrow(my_excel),], header = FALSE, check.names = FALSE)
    print(dim(df))
    names(df) <- my_names
    print(dim(df))
    #### Parsing for day, meal, study intervention, and food item ####
    #the order of the if statements is important because those that start with 
    #12 spaces also have 4 spaces, so we must start with the 12 spaces.
    for (item in 1:nrow(df)){
      my_food <- df$`Item Name`[item]
      # print(my_food)
      if (startsWith(my_food, "Day ")) {
        day <- my_food
      }else{
        my_whitespace <- paste(rep(" ", 12), collapse = "")
        if (substr(my_food, 1,12) == my_whitespace){
          my_item <- gsub(my_whitespace, "", my_food)
          # print(paste(day, meal, dish, my_item))
          my_row <- c(my_file, my_sheet, day, meal, recipe, my_item, df[item,2:ncol(df)])
          out_df[rows_added,] <- my_row
          rows_added <- rows_added + 1
        }else{
          my_whitespace <- paste(rep(" ", 8), collapse = "")
          if (substr(my_food, 1,8) == my_whitespace){
            recipe <- gsub(my_whitespace, "", my_food)
            #some items are indented like recipes, so adding logic to add them
            next_row <- item + 1
            if (next_row <= nrow(df)){#make sure that there's another line to read
              next_food <- df$`Item Name`[next_row]
              my_whitespace <- paste(rep(" ", 12), collapse = "")
              if (substr(next_food, 1,12) == my_whitespace){}
              else{
                my_item <- recipe
                recipe <- "no recipe"
                my_row <- c(my_file, my_sheet, day, meal, recipe, my_item, df[item,2:ncol(df)])
                out_df[rows_added,] <- my_row
                rows_added <- rows_added + 1
              }
            }else{#for when the df ends on a recipe item
              my_item <- recipe
              recipe <- "no recipe"
              my_row <- c(my_file, my_sheet, day, meal, recipe, my_item, df[item,2:ncol(df)])
              out_df[rows_added,] <- my_row
              rows_added <- rows_added + 1
            }
          }else{
            my_whitespace <- paste(rep(" ", 4), collapse = "")
            if (substr(my_food, 1,4) == my_whitespace){
              meal <- gsub(my_whitespace, "", my_food)
              my_quant <- df$Quantity[item]
              if (!is.na(my_quant)){
                my_item <- meal
                recipe <- "no recipe"
                my_row <- c(my_file, my_sheet, day, meal, recipe, my_item, df[item,2:ncol(df)])
                out_df[rows_added,] <- my_row
                rows_added <- rows_added + 1
                
              }
            }
          }
        }
      }
    }
  }#for sht
}#for ef

out_df[out_df == "-"] <- NA
out_df[out_df == "--"] <- NA
out_df$entry_num <- row.names(out_df) #makes a column that counts items as added
out_df <- out_df[,c(ncol(out_df),1:(ncol(out_df)-1))]#move last column to first

#rank columns for NA - helps determine the columns to use downstream
my_na <- sort(colSums(is.na(out_df)), decreasing = TRUE)/nrow(out_df)*100
print(my_na)

#remove white space
out_df <- data.frame(apply(out_df, MARGIN = 2, FUN = trimws))

#TODO convert numeric columns back to numeric
# str_df <- sapply(out_df, as.numeric)
# str_df <- data.frame(lapply(out_df, as.numeric))

# write.table(out_df, file = file.path(data_dir, "combined_esha_studies.tsv"), 
#           sep = "\t", row.names = F)
data.table::fwrite(out_df, file = file.path(data_dir, "combined_esha_studies.tsv"), 
                             sep = "\t", row.names = F)

#Reading in and out to strip leading and trailing whitespace
out_df1 <- data.table::fread(file = file.path(data_dir, "combined_esha_studies.tsv"))

