# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for organizing dietary data from MB

#data from USDA has these columns: "Study", "Intervention", "Day", "Meal", "Recipe", "Item.Name"

# Example of part of a spreadsheet:
# Spreadsheet: Day - MB-2112_SC_001 - Day 1							
# Spreadsheet							
# Item Name	Quantity	Measure	Wgt (g)	Cals (kcal)	FatCals (kcal)	SatCals (kcal)	Prot (g)
# Morning Snack			24	100	36	9	1
# Quaker, Chewy Chocolate Chip, granola bar	24	Gram	24	100	36	9	1
# Morning Snack Adjusted Avg			24	100	36	9	1
# Lunch			128.53	265.16	42.74	0	16.25
# ham, slow roasted, Carving Board	42.525	Gram	42.52	45	6.75	0	8.25
# bread, 100% whole wheat, Brownberry, 1 lb 8 oz	86	Gram	86	220.16	35.99	0	8
# Lunch Adjusted Avg			128.53	265.16	42.74	0	16.25
# Dinner			350	551.62	169	44.07	54.49
# rice, white, cooked	180	Gram	180	254.12	85.76	28.59	4.24
# chicken breast quarter, baked, skin removed	170	Gram	170	297.5	83.23	15.48	50.25
# Dinner Adjusted Avg			350	551.62	169	44.07	54.49
# Breakfast			309.7	623.86	276.69	80.23	39.04
# Oscar Mayer, naturally hardwood smoked bacon	19	Gram	19	90	63	22.5	7
# egg, brown, raw	200	Gram	200	280	144	36	24
# butter, Land O Lakes, salted, stick	4.7	Gram	4.7	33.7	33.7	21.73	0.04
# bread, 100% whole wheat, Brownberry, 1 lb 8 oz	86	Gram	86	220.16	35.99	0	8
# Breakfast Adjusted Avg			309.7	623.86	276.69	80.23	39.04
# Day 1 Adjusted Average			812.23	1540.64	524.43	133.3	110.77
# Day 1 Percent Standard							
# 
# Spreadsheet: Day - MB-2112_SC_001 - Day 2							
# Spreadsheet							
# Item Name	Quantity	Measure	Wgt (g)	Cals (kcal)	FatCals (kcal)	SatCals (kcal)	Prot (g)

rm(list = ls()) #clear workspace

print(paste("Working in", getwd()))

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("readxl", quietly = TRUE))  BiocManager::install("readxl")
library("readxl")

print("Loaded packages")

#### Establish directory layout and other constants ####
base_dir <- file.path("data", "diet")
mb_diet <- file.path(base_dir, "mb", "2112 Results.xlsx")

#### Loading in data ####
xl_wb <- getSheetNames(mb_diet)
diet_df <- openxlsx::read.xlsx(mb_diet)
print(names(xl_wb))

meal_keywords <- c("Breakfast", "Morning Snack", "Lunch", "Dinner", "Afternoon Snack")

#data from USDA has these columns: "Study", "Intervention", "Day", "Meal", "Recipe", "Item.Name"
nutr_columns <- 2:64
nutr_column_names <- nutr_column_names <- diet_df[2,nutr_columns]
out_names <- c("Study", "Intervention", "Day", "Meal", "Recipe", "Item Name", nutr_column_names)
out_df <- data.frame(matrix(1:length(out_names), ncol = length(out_names)))
names(out_df) <- out_names
rows_added <- 1


for (wb in xl_wb){
  # print(wb)
  diet_df <- readxl::read_xlsx(mb_diet, sheet = wb, skip = 2, col_names = TRUE)
  day_count <- 1
  current_meal <- NA
  nutr_column_names <- diet_df[2,nutr_columns]
  intervention <- paste0("MB-2112_", wb)
  for (i in 1:nrow(diet_df)){
    item <- diet_df[i,1]
    if (any(item %in% meal_keywords)){
      for (mk in meal_keywords){
        if (item == mk){
          # print(mk)
          current_meal <- mk
        }
      }
    }else{
      next_day_pattern <- paste0("Spreadsheet: Day - MB-2112_", wb)
      # Spreadsheet: Day - MB-2112_SC_001 - Day 2
      if (grepl(pattern = next_day_pattern, x = item)) {
        print("BIG ELSE")
        day_count <- day_count + 1
      } else{
        
      if (!is.na(diet_df[i,2]) & item != "Item Name"){
        # print(item, day_count)
        # "Study", "Intervention", "Day", "Meal", "Recipe", "Item Name", nutr_column_names
        nut_data <- diet_df[i, nutr_columns]
        my_row <- c("MB-2112", wb, day_count, current_meal, NA, item, nut_data)
        out_df[rows_added,] <- my_row
        rows_added <- rows_added + 1
        }
      }
    }
  }
}

write.table(out_df,
          file = file.path(base_dir, "nutrition_data", "mb_2112_esha.tsv"),
          sep="\t")

print("End org_MB_dietary.R")

