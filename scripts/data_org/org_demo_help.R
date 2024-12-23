# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for organizing demographic metadata

# USDA Studies
# There are two separate files that contain metadata for the MED study:
#   1 PSU demo.xlsx - contains Sex,	BMI, and Age of PSU portion of med study
#   2 USDA PSU Med Beef Data.xlsx - contains all CVD markers of med study and Sex	BMI	Age
#  of USDA portion

rm(list = ls()) #clear workspace

print(paste("Working in", getwd()))

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("openxlsx", quietly = TRUE))  BiocManager::install("openxlsx")
library("openxlsx")
if (!require("ggplot2")) BiocManager::install("ggplot2")
library("ggplot2")

print("Loaded packages")

#### Establish directory layout and other constants ####
base_dir <- file.path("data", "metabolomics", "UARS-01-23ML+")
metabo_f <- file.path(base_dir,
                      "UARS-01-23ML+ DATA TABLES (DATA ADJUSTED BY BASELINE SAMPLES FROM EACH SITE).xlsx")
metabo_f_new <- file.path(base_dir,"NCBA_siteCorrected_mod.xlsx")

#### Loading in data ####
# Original sample metadata to get ids and sites
meta_df <- openxlsx::read.xlsx(metabo_f, sheet = "Sample Meta Data")

# Demographic data
psu_med_demo_df <- openxlsx::read.xlsx(file.path("data","mapping","PSU demo.xlsx"))
usda_med_df <- openxlsx::read.xlsx(file.path("data","mapping","USDA PSU Med Beef Data.xlsx"))
mb_meta <- read.csv(file.path("data","mapping","mb", "2112_V1   .csv"), 
                    check.names = FALSE)
mb2_meta <- read.csv(file.path("data","diet","mb", "2112 V2 .csv"), 
                     check.names = FALSE)
mb_map <- openxlsx::read.xlsx(file.path("data","mapping","mb", "MB-2112_Screen and Rand.xlsx"))
purdue_meta <- openxlsx::read.xlsx(file.path("data","mapping", "purdue", "AY_sample_codebook 9.12.2024 Campbell data.xlsx"),
                                   sheet = "Subject Characteristics",
                                   sep.names = " ")
purdue_meta$short_id <- sapply(strsplit(purdue_meta$`Participant ID`, "-"), "[", 3)#take 3rd part of split

# Add psu data to usda df
psu_med_demo_df <- psu_med_demo_df[!duplicated(psu_med_demo_df), ]
if (length(unique(psu_med_demo_df$Subject)) == nrow(psu_med_demo_df)) print("unique ids")
for (rw in seq_along(1:nrow(psu_med_demo_df))){
  id <- psu_med_demo_df$Subject[rw]
  usda_med_df[usda_med_df$Subject == id, "Age"] <- psu_med_demo_df$Age[rw]
  usda_med_df[usda_med_df$Subject == id, "BMI"] <- psu_med_demo_df$BMI[rw]
  usda_med_df[usda_med_df$Subject == id, "Sex"] <- psu_med_demo_df$Sex[rw]
}

#save full combined MED study metadata
write.csv(usda_med_df, file = file.path("data", "mapping", "usda_full_metadata.csv"),
          row.names = FALSE)

#### Iterate through each row and add metadata from site dfs ####
meta_df <- meta_df[,c("PARENT_SAMPLE_NAME", "CLIENT_IDENTIFIER", "CLIENT_SAMPLE_ID", "SITE", "CORRECTED_SITE")]

age <- vector(mode="integer", length = nrow(meta_df))
bmi <- vector(mode="numeric", length = nrow(meta_df))
sex <- vector(mode="character", length = nrow(meta_df))

for (i in seq_along(1:nrow(meta_df))){
  id <- meta_df$CLIENT_SAMPLE_ID[i]
  site <- meta_df$CORRECTED_SITE[i]
  print(paste(id, site))
  if (site %in% c("USDA-MED", "PSU-MED")){
    my_row <- which(usda_med_df$Subject == id)[1]
    print(paste(my_row, id))
    age[i] <- usda_med_df[my_row, "Age"]
    bmi[i] <- usda_med_df[my_row, "BMI"]
    sex[i] <- usda_med_df[my_row, "Sex"]
  }
  if (site == "USDA-MAP" ){
    age[i] <- NA
    bmi[i] <- NA
    sex[i] <- NA
  }
  if (site == "MB/IIT"){
    mb_id <- mb_map[mb_map$Randomization == id, "Screen"]
    my_row <- which(mb_meta$Screen == mb_id)[1]
    birthyear <- mb_meta[my_row, "Year_Birth"]
    visit_date <- as.numeric(strsplit(mb_meta[my_row,"Visit_Date"], "/")[[1]][3])
    age[i] <- visit_date - birthyear
    bmi[i] <- mb_meta[my_row, "BMI"]
    if (mb_meta[my_row, "Non_applicable"] == "Male"){
      sex[i] <- "M"}
    else{ sex[i] <- "F"}
  }
  if (site == "Purdue"){
    my_row <- which(purdue_meta$short_id == id)[1]
    age[i] <- purdue_meta[my_row, "Age"]
    bmi[i] <- purdue_meta[my_row, "BMI"]
    sex[i] <- purdue_meta[my_row, "Sex"]
  }
}

sex[sex == "M"] <- 1
sex[sex == "F"] <- 0

meta_df$age <- age
meta_df$bmi <- bmi
meta_df$sex <- sex

meta_demo <- meta_df[,c("PARENT_SAMPLE_NAME", "age", "bmi", "sex")]

write.csv(meta_demo, file = file.path("data", "mapping", "full_demo.csv"),
          row.names = FALSE)
meta_df <- meta_df[meta_df$CORRECTED_SITE != "USDA-MAP",]

meta_df <- meta_df[,c("PARENT_SAMPLE_NAME", "age", "bmi", "sex")]

write.csv(meta_df, file = file.path("data", "mapping", "noMap_demo.csv"),
          row.names = FALSE)


print("End R script.")

