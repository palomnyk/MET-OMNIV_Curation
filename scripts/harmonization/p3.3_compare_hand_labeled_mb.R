#Author: Aaron Yerke (aaronyerke@gmail.com)
#Script for comparing my and Rita's selection for FPED labels and meat type labels

if (!requireNamespace("readxl", quietly = TRUE))  BiocManager::install("readxl")
library("readxl")

out_file <- file.path("data", "diet", "mb", "mb_FPED_mismatch.txt")

#### Read in data ####
aaron_label <- read.csv("data/diet/mb/mb-unique_esha_fped_AY.tsv", sep = "\t",
                        check.names = FALSE)
rita_label <- read.csv("data/diet/mb/mb-unique_esha_fped_RR_3-11-25_AY.tsv",
                       sep = "\t", check.names = FALSE)
fped <- readxl::read_excel("data/diet/nutrition_data/FPED_1720.xls")

#### Add food description to Aaron's table ####
food_dscr <- vector(mode = "character", length = nrow(aaron_label))
for (rw in 1:nrow(aaron_label)){
  fc <- aaron_label$food_code[rw]
  if (! is.na(fc)){
    fd <- fped$DESCRIPTION[which(fped$FOODCODE == fc)]
    food_dscr[rw] <- fd
  }
}

aaron_label$`food_name/description` <- food_dscr

#### Compare meat manual labeling ####
#labeling just the type of meat and whether it is processed or min processed
check_meats <- c("beef", "chicken", "pork","turkey",
                "processed")
check_names <- c("oz-g_equiv", "food_code", "food_name/description")

# Now build from hand labeled, taking advantage of the fact that Lauren's start
# with a capital and mine start with a lowercase, but spelled the same.

file.create(out_file)
fileConn<-file(out_file)
writeLines("THE FOLLOWING ARE DIFFERENCES IN THE MEAT TYPES", fileConn)
type_counts <- 0
for (rw in 1:nrow(aaron_label)){
  a_meat_types <- paste(aaron_label[rw, check_meats], collapse = "|")
  r_meat_types <- paste(rita_label[rw, check_meats], collapse = "|")
  if (!identical(a_meat_types, r_meat_types)){
    type_counts <- type_counts + 1
    write(paste("count:", type_counts, "ROW:", rw, "A:", a_meat_types, "R:", r_meat_types, "Food:", aaron_label$`Item Name`[rw]), 
          file = file.path(out_file),
          append = TRUE)
  }
}


code_counts <- 0
for (rw in 1:nrow(aaron_label)){
  a_meat_types <- paste(aaron_label[rw, check_names], collapse = "|")
  r_meat_types <- paste(rita_label[rw, check_names], collapse = "|")
  if (!is.na(aaron_label$`oz-g_equiv`[rw]) | !is.na(rita_label$`oz-g_equiv`[rw])){
    if (aaron_label$`oz-g_equiv`[rw] != rita_label$`oz-g_equiv`[rw] | any(is.na(c(aaron_label$`oz-g_equiv`[rw], rita_label$`oz-g_equiv`[rw])))){
      code_counts <- code_counts + 1
      # print(paste("count:", code_counts, "ROW:", rw, "A:", a_meat_types, "R:", r_meat_types, "Food:", aaron_label$`Item Name`[rw]))
      write(paste("count:", code_counts, "ROW:", rw, "A:", a_meat_types, "R:", r_meat_types, "Food:", aaron_label$`Item Name`[rw]),
            file = file.path(out_file),
            append = TRUE)
    }
  }
}
# close(fileConn)

print("End of R Script")
