# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for organizing CVD markers
# Notes:
# Starting with dietary data file

rm(list = ls()) #clear workspace

print(paste("Working in", getwd()))

#### Load dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("openxlsx", quietly = TRUE))  BiocManager::install("openxlsx")
library("openxlsx")
if (!require("ggplot2")) BiocManager::install("ggplot2")
library("ggplot2")
if (!requireNamespace("readxl", quietly = TRUE))  BiocManager::install("readxl")
library("readxl")

print("Loaded packages")

#### Establish directory layout and other constants ####
out_dir <- file.path("data", "CVD", "modified")

##### Loading in data #####
meta_df <- read.csv("data/mapping/all_sites-auto_protn_metadata.csv",
                    check.names = FALSE)
mb_cvd_df <- as.data.frame(
  readxl::read_excel(file.path("data","CVD", "raw", "MB 2112 Insulin_Lipid_03_17_25.xlsx")),
  check.names=FALSE)
mb_map <- as.data.frame(
  readxl::read_excel(file.path("data","mapping","mb", "MB-2112_Screen and Rand.xlsx")),
  check.names=FALSE)
med_df <- as.data.frame(
  readxl::read_excel(file.path("data","CVD", "raw", "Med Beef Data for A Yerke 03 20 2025.xlsx")),
  check.names=FALSE)
purd_df <- as.data.frame(
  readxl::read_excel(file.path("data","mapping", "purdue", "AY_sample_codebook 9.12.2024 Campbell data.xlsx"),
                     sheet = "Blood, BP< Anthropemetrics"),
  check.names=FALSE)
control_treatments <- c("Med 100","Purple", "Orange","Med 200","USUAL", "C")
correct_sites <- c("PSU-MED","MB/IIT","Purdue","USDA-MAP","USDA-MED")

unknown_beef <- c("Blue","Red","Green", "Yellow", "baseline", "BL")

#### Reorganize for easier usage ####
##### Midwest Biologicals #####
mb_cvd_df[mb_cvd_df == "1-Screening"] <- 1
mb_cvd_df$Visit <- as.numeric(mb_cvd_df$Visit)

##### USDA/PSU MED #####
med_df$`LDL mg/dL` <- as.numeric(med_df$`LDL mg/dL`)

##### Purdue #####
purd_df[purd_df$Diet == "A", "Diet"] = "MED"
purd_df[purd_df$Diet == "B", "Diet"] = "VEG"

#### Main work: coalate CVD markers ####
ldl <- vector(mode = "numeric", length = nrow(meta_df))

meta_df$mb_screen <- rep(NA, nrow(meta_df))
for (i in seq_along(1:nrow(meta_df))){
  id <- meta_df$CLIENT_SAMPLE_ID[i]
  site <- meta_df$SITE[i]
  treat <- meta_df$TREATMENT[i]
  tp <- meta_df$TIMEPOINT[i]
  print(paste(id, site, treat))
  if (site %in% c("USDA-MED", "PSU-MED")){
    med_row <- which(med_df$Subject == id & med_df$`Diet Name` == treat)
    ldl[i] <- med_df[med_row, "LDL mg/dL"]
  }
  if (site == "USDA-MAP" ){
    ldl[i] <- NA
  }
  if (site == "MB/IIT"){
    mb_scrn <- mb_map[mb_map$Randomization == id, "Screen"]
    mb_cvd_id <- paste0("MB2112_", formatC(id, width=3, flag="0"))
    mb_visit <- meta_df$CHRONOLOGICAL_TIMEPOINT[i]
    meta_df$mb_screen[i] <- mb_scrn
    my_ldl <- mb_cvd_df[mb_cvd_df$`Subject ID` == mb_cvd_id &
                          mb_cvd_df$Visit == mb_visit &
                          (mb_cvd_df$Timepoint == -15 |
                             mb_cvd_df$Timepoint == 0), "LDL (mg/dl)"]
    if (any(!is.na(my_ldl))){
      ldl[i] <- my_ldl[which(!is.na(my_ldl))]
    }else{
      ldl[i] <- NA
    }
  }
  if (site == "Purdue"){
    my_ldl <- purd_df[purd_df$StudyID == id & purd_df$Diet == treat &
                        purd_df$Time == tp, "LDL"]
    
    if (length(my_ldl) > 0){
      ldl[i] <- my_ldl
    }else{
      ldl[i] <- NA
    }
  }
}

meta_df$ldl <- ldl

agg_columns <- c("ldl")

base_columns <- c()
#### Save output ####
for (site in unique(meta_df$SITE)){
  clean_site <- gsub("/", "_", site)
  clean_site <- gsub("-", "_", clean_site)
  sub_df <- meta_df[meta_df$SITE == site,]
  
  fname <- paste0(clean_site, "-", "cvd_markers.csv")
  write.csv(sub_df, file = file.path(out_dir, fname),
            row.names = FALSE)
  
  # Remove LCMS technical data for testing in random forest
  # meta_df <- meta_df[c("PARENT_SAMPLE_NAME", "SITE","TIMEPOINT","TREATMENT", agg_columns, names(new_rows))]
  sub_df <- sub_df[c("PARENT_SAMPLE_NAME", agg_columns)]
  fname <- paste0(clean_site, "-", "rf_cvd_markers.csv")
  write.csv(sub_df, file = file.path(out_dir, fname),
            row.names = FALSE)
  
}
write.csv(meta_df, file = file.path(out_dir, "all_sites-cvd_markers.csv"),
          row.names = FALSE)

meta_df <- meta_df[meta_df$SITE != "Map",]
write.csv(meta_df, file = file.path(out_dir, "noMap-cvd_markers.csv"),
          row.names = FALSE)

#### Make non-mb-non-map dataset ####
sub_df <- meta_df[meta_df$SITE != "MB/IIT",]
write.csv(sub_df, file = file.path(out_dir, "noMB_noMap-cvd_markers.csv"),
          row.names = FALSE)
sub_df <- sub_df[c("PARENT_SAMPLE_NAME", "SITE","TREATMENT", agg_columns)]
write.csv(sub_df, file = file.path(out_dir, "noMB_noMap-rf_cvd_markers.csv"),
          row.names = FALSE)

# Remove LCMS technical data for testing in random forest
# meta_df <- meta_df[c("PARENT_SAMPLE_NAME", "SITE","TIMEPOINT","TREATMENT", agg_columns, names(new_rows))]
meta_df <- meta_df[c("PARENT_SAMPLE_NAME", "SITE","TREATMENT", agg_columns)]
write.csv(meta_df, file = file.path(out_dir, "noMap-rf_cvd_markers.csv"),
          row.names = FALSE)

print("End R script.")


