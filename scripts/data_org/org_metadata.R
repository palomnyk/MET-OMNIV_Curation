# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for organizing metadata and labeling meet eaten from samples

# For MB:
  # "screen" in the MB documentation matches to "SAMPLE_ID" in metabolomic metadata
  # The food is labeled in the "Study_Product_Dispensed" of the "2112 Vx.csv" where
  # "x" is the leg of the trial. This is labeled as "A" or "B" in the metabolomic
  # metadata for 6.5oz of either beef and chicken respectively.
  # See the output for screen 11 below:
  # [1] "11|**|MB2112: V2 SUB.ID: 011 IL-6 T=(-15)|**|MB/IIT|**|A"
  # Beef
  # [1] "11|**|MB2112: V3 SUB.ID: 011 IL-6 T=(-15)|**|MB/IIT|**|A"
  # Beef
  # [1] "11|**|MB2112: V4 SUB.ID: 011 IL-6 Archive|**|MB/IIT|**|B"
  # Chicken
  # [1] "11|**|MB2112: V5 SUB.ID: 011 IL-6 T=(-15)|**|MB/IIT|**|B"

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
meta_df <- openxlsx::read.xlsx(metabo_f,
                               sheet = "Sample Meta Data")
control_treatments <- c("Med 100","C","Purple", "Orange","Med 200","USUAL")
correct_sites <- c("PSU-MED","MB/IIT","Purdue","USDA-MAP","USDA-MED")
xl_wb <- openxlsx::loadWorkbook(metabo_f) #excel workbook
print(names(xl_wb))
all_xl <- c("UARS-01-23ML+ DATA TABLES (ALL SAMPLES).xlsx",
            "UARS-01-23ML+ DATA TABLES (CITRATE PLASMA SAMPLES).xlsx",
            "UARS-01-23ML+ DATA TABLES (DATA ADJUSTED BY BASELINE SAMPLES FROM EACH SITE).xlsx",
            "UARS-01-23ML+ DATA TABLES (EDTA PLASMA SAMPLES).xlsx")

# Beef level related
high_beef <- c("Med 2.5", "Med 5.5", "AAD", "A", "MED")
low_beef <- c("Med 0.5", "B")
no_beef <- c("VEG")
unknown_beef <- c("Blue","Red","Green", "Yellow", "baseline", "BL", "Control")

#### Check if all metadata files are the same ####
for (xl in all_xl){
  test_df <- openxlsx::read.xlsx(file.path(base_dir, xl),
                                 sheet = "Sample Meta Data",
                                 rowNames = TRUE)
  if (identical(test_df, meta_df)){
    print(paste(xl, "is identical to", metabo_f))
  }else{
    print(paste(xl, "NOT identical", metabo_f))
  }
}
#### Fix some inconsistencies ####
meta_df$controls <- meta_df$TREATMENT %in% control_treatments
meta_df[meta_df == "MED 0.5"] <- "Med 0.5"
meta_df[meta_df == "MED 2.5"] <- "Med 2.5"
meta_df[meta_df == "MED 5.5"] <- "Med 5.5"
meta_df[meta_df == "post "] <- "post"

#### Create "control" columns ####
meta_df$TREATMENT[meta_df$TREATMENT %in% control_treatments] <- "Control"

#to make three beef level categories
beef_col <- meta_df$TREATMENT
for (treat in high_beef){
  print(treat)
  beef_col[which(beef_col == treat)] <- "high_beef"
}
for (treat in low_beef){
  print(treat)
  beef_col[which(beef_col == treat)] <- "low_beef"
}
for (treat in no_beef){
  print(treat)
  beef_col[which(beef_col == treat)] <- "no_beef"
}
for (treat in unknown_beef){
  print(treat)
  beef_col[which(beef_col == treat)] <- "unknown"
}

meta_df$beef_level <- beef_col

#### Create column for oz of beef ####
beef_col <- meta_df$TREATMENT
for (treat in unknown_beef){
  print(treat)
  beef_col[which(beef_col == treat)] <- NA
}
for (treat in no_beef){
  print(treat)
  beef_col[which(beef_col == treat)] <- 0
}
beef_col[which(beef_col == "Med 5.5")] <- 5.5
beef_col[which(beef_col == "Med 2.5")] <- 2.5
beef_col[which(beef_col == "Med 0.5")] <- 0.5
beef_col[which(beef_col == "AAD")] <- 5.5
beef_col[which(beef_col == "A")] <- 6.5
beef_col[which(beef_col == "B")] <- 0.5
beef_col[which(beef_col == "MED")] <- 3

meta_df$beef_level_oz <- beef_col

#### Create columns for types of meat ####
turkey <- vector(mode = "numeric", length = nrow(meta_df))
chicken <- vector(mode = "numeric", length = nrow(meta_df))
beef <- vector(mode = "numeric", length = nrow(meta_df))
lamb <- vector(mode = "numeric", length = nrow(meta_df))
pork <- vector(mode = "numeric", length = nrow(meta_df))

mb_base_dir <- file.path("data","diet","mb")
mb_table_V2 <- read.csv(file.path(mb_base_dir, "2112 V2 .csv"))
mb_table_V3 <- read.csv(file.path(mb_base_dir, "2112 V3    .csv"))
for (rw in 1:nrow(meta_df)){
  samp_id <- meta_df$CLIENT_SAMPLE_ID[rw]
  full_id <- meta_df$CLIENT_IDENTIFIER[rw]
  site <- meta_df$CORRECTED_SITE[rw]
  treat <- meta_df$TREATMENT[rw]
  # print(paste(samp_id, full_id, site, treat, secp = "|**|"))
  
  if (site == "MB/IIT"){
    print("MB/IIT")
    study_grp <- strsplit(full_id, ":", )
    print(paste(samp_id, full_id, site, treat, sep = "|**|"))
    if(treat == "A"){
      beef[rw] <- 6.5
    }
    if(treat == "B"){
      chicken[rw] <- 6.5
    }
  }else{
    
  }
}


write.csv(meta_df, file = file.path("data", "mapping", "full_metadata.csv"),
          row.names = FALSE)

meta_df <- meta_df[meta_df$beef_level != "unknown",]

write.csv(meta_df, file = file.path("data", "mapping", "noMap_metadata.csv"),
          row.names = FALSE)

# Remove LCMS techincal data for testing in random forest
meta_df <- meta_df[,c("PARENT_SAMPLE_NAME", "SITE", "TIMEPOINT","TREATMENT","controls","beef_level","beef_level_oz")]

write.csv(meta_df, file = file.path("data", "mapping", "rf_noMap_metadata.csv"),
          row.names = FALSE)

print("End R script.")

