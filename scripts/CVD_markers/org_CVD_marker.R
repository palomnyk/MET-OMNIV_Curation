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
meta_df <- read.csv("data/mapping/all_sites-meats.csv",
                    check.names = FALSE)
mb_cvd_df <- as.data.frame(
  readxl::read_excel(file.path("data","CVD", "raw", "mb", "MB 2112 Insulin_Lipid_03_17_25.xlsx")),
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
LDL = vector(mode = "numeric", length = nrow(meta_df))
Total_cholesterol = vector(mode = "numeric", length = nrow(meta_df))
Triglycerides = vector(mode = "numeric", length = nrow(meta_df))
dHDL = vector(mode = "numeric", length = nrow(meta_df))
Avg_systolic = vector(mode = "numeric", length = nrow(meta_df))
Avg_dystolic = vector(mode = "numeric", length = nrow(meta_df))

cvd_markers <- data.frame(
  LDL, Total_cholesterol, Triglycerides, dHDL, Avg_dystolic, Avg_systolic)

meta_df$mb_screen <- rep(NA, nrow(meta_df))
for (i in seq_along(1:nrow(meta_df))){
  id <- meta_df$CLIENT_SAMPLE_ID[i]
  site <- meta_df$SITE[i]
  treat <- meta_df$TREATMENT[i]
  tp <- meta_df$TIMEPOINT[i]
  samp_id <- meta_df$CLIENT_IDENTIFIER[i]
  t(paste(id, site, treat))
  if (site %in% c("USDA-MED", "PSU-MED")){
    med_row <- which(med_df$Subject == id & med_df$`Diet Name` == treat)
    cvd_markers$LDL[i] <- med_df[med_row, "LDL mg/dL"]
    cvd_markers$Total_cholesterol[i] <- med_df[med_row, "CHOL mg/dL"]
    cvd_markers$Triglycerides[i] <- med_df[med_row, "TRIG mg/dL"]
    cvd_markers$dHDL[i] <- med_df[med_row, "dHDL mg/dL"]
    cvd_markers$Avg_systolic[i] <- med_df[med_row, "SystolicBP"]
    cvd_markers$Avg_dystolic[i] <- med_df[med_row, "Diastolic BP"]
  }
  if (site == "USDA-MAP" ){

  }
  if (site == "MB/IIT"){
    mb_scrn <- mb_map[mb_map$Randomization == id, "Screen"]
    meta_df$mb_screen[i] <- mb_scrn
    mb_cvd_id <- paste0("MB2112_", formatC(id, width=3, flag="0"))
    mb_visit <- meta_df$CHRONOLOGICAL_TIMEPOINT[i]

    my_row <- which(mb_cvd_df$`Subject ID` == mb_cvd_id &
                      mb_cvd_df$Visit == mb_visit &
                      (mb_cvd_df$Timepoint == -15 |
                         mb_cvd_df$Timepoint == 0))
    if(length(my_row) > 0){
      ##### Extract lipid panel #####
      cvd_markers$LDL[i] <- mb_cvd_df[my_row, "LDL (mg/dl)"]
      cvd_markers$Total_cholesterol[i] <- mb_cvd_df[my_row, "Cholesterol (mg/dl)"]
      cvd_markers$Triglycerides[i] <- mb_cvd_df[my_row, "Triglycerides (mg/dl)"]
      cvd_markers$dHDL[i] <- mb_cvd_df[my_row, "Direct HDL (mg/dl)"]
      
    }
    
    ##### Extract blood presure data #####
    new_df_name <- paste0("2112 V",mb_visit + 1, ".csv")
    new_mb_df <- read.csv(file.path("data","CVD","raw","mb", new_df_name),
                       check.names = FALSE)
    
    mb_bp_row <- which(new_mb_df$Screen == mb_scrn)
    cvd_markers$Avg_systolic[i] <- new_mb_df[mb_bp_row, "Systolic_Average"]
    cvd_markers$Avg_dystolic[i] <- new_mb_df[mb_bp_row, "Diastolic_Average"]
  }
  if (site == "Purdue"){
    tp <- ifelse(tp == "baseline", "pre", "post")
    my_row <- which(purd_df$StudyID == id & purd_df$Diet == treat &
                      purd_df$Time == tp)
    print(my_row)
    if (length(my_row) > 0){
      cvd_markers$LDL[i] <- purd_df[my_row, "LDL"]
      cvd_markers$Total_cholesterol[i] <- purd_df[my_row, "TC"]
      cvd_markers$Triglycerides[i] <- purd_df[my_row, "TG"]
      cvd_markers$dHDL[i] <- purd_df[my_row, "HDL"]
      cvd_markers$Avg_systolic[i] <- purd_df[my_row, "Avg_SBP"]
      cvd_markers$Avg_dystolic[i] <- purd_df[my_row, "Avg_DBP"]
    }
  }
}

agg_columns <- names(cvd_markers)

cvd_markers[cvd_markers == "."] <- NA
cvd_markers[cvd_markers == 0 ] <- NA

meta_df <- cbind(meta_df, cvd_markers)

#### Save output ####
for (site in unique(meta_df$SITE)){
  clean_site <- gsub("/", "_", site)
  clean_site <- gsub("-", "_", clean_site)
  sub_df <- meta_df[meta_df$SITE == site,]
  
  fname <- paste0(clean_site, "-", "cvd_marker_meat_meta.csv")
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
sub_df <- meta_df[c("PARENT_SAMPLE_NAME", "SITE", agg_columns)]
write.csv(sub_df, file = file.path(out_dir, "noMap-cvd_markers_site.csv"),
          row.names = FALSE)

#### Make non-mb-non-map dataset ####
sub_df <- meta_df[meta_df$SITE != "MB/IIT",]
write.csv(sub_df, file = file.path(out_dir, "noMB_noMap-cvd_markers.csv"),
          row.names = FALSE)
sub_df <- sub_df[c("PARENT_SAMPLE_NAME", agg_columns)]
write.csv(sub_df, file = file.path(out_dir, "noMB_noMap-rf_cvd_markers.csv"),
          row.names = FALSE)

# Remove LCMS technical data for testing in random forest
# meta_df <- meta_df[c("PARENT_SAMPLE_NAME", "SITE","TIMEPOINT","TREATMENT", agg_columns, names(new_rows))]
meta_df <- meta_df[c("PARENT_SAMPLE_NAME", agg_columns)]
write.csv(meta_df, file = file.path(out_dir, "noMap-rf_cvd_markers.csv"),
          row.names = FALSE)

print("End R script.")


