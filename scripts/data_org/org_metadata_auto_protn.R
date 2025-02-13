# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for organizing auto labeled protein categories
# Notes:
#   Drop "SITE" column and change "CORRECTED_SITE" to "SITE"

rm(list = ls()) #clear workspace

print(paste("Working in", getwd()))

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("openxlsx", quietly = TRUE))  BiocManager::install("openxlsx")
library("openxlsx")
if (!require("ggplot2")) BiocManager::install("ggplot2")
library("ggplot2")
if (!requireNamespace("readxl", quietly = TRUE))  BiocManager::install("readxl")
library("readxl")

print("Loaded packages")

#### Establish directory layout and other constants ####
base_dir <- file.path("data", "metabolomics", "UARS-01-23ML+")
nut_dir <- file.path("data", "diet", "nutrition_data")
metabo_f <- file.path(base_dir,
                      "UARS-01-23ML+ DATA TABLES (DATA ADJUSTED BY BASELINE SAMPLES FROM EACH SITE).xlsx")
metabo_f_new <- file.path(base_dir,"NCBA_siteCorrected_mod.xlsx")
meat_thresholds <- c(0.0, 70) #high beef was 2.5 oz or higher, 2.5 *28 == 69.72 round to 70
meat_threshold_names <- c("no", "low", "high")

#### Loading in data ####
meta_df <- openxlsx::read.xlsx(metabo_f,
                               sheet = "Sample Meta Data")
mb_auto_protn_df <- read.csv(file = file.path(nut_dir ,"mb-esha_combined_meats_HEI_vals.tsv"),
                             sep = "\t", check.names = FALSE)
mb_map <- openxlsx::read.xlsx(file.path("data","mapping","mb", "MB-2112_Screen and Rand.xlsx"))
# usda_auto_protn_df <- read.csv(file = file.path(nut_dir ,"esha_combined_meats_HEI_vals.tsv"),
#                                sep = "\t", check.names = FALSE)
usda_auto_protn_df <- read.csv(file = file.path(nut_dir, "esha_combined_meats_HEI_vals28Sep2023.tsv"),
                               sep = "\t", check.names = FALSE)
control_treatments <- c("Med 100","Purple", "Orange","Med 200","USUAL", "C")
correct_sites <- c("PSU-MED","MB/IIT","Purdue","USDA-MAP","USDA-MED")
xl_wb <- openxlsx::loadWorkbook(metabo_f) #excel workbook
print(names(xl_wb))
all_xl <- c("UARS-01-23ML+ DATA TABLES (ALL SAMPLES).xlsx",
            "UARS-01-23ML+ DATA TABLES (CITRATE PLASMA SAMPLES).xlsx",
            "UARS-01-23ML+ DATA TABLES (DATA ADJUSTED BY BASELINE SAMPLES FROM EACH SITE).xlsx",
            "UARS-01-23ML+ DATA TABLES (EDTA PLASMA SAMPLES).xlsx")

# Beef level related
high_beef <- c("Med 2.5", "Med 5.5", "AAD", "Beef diet", "MED")
low_beef <- c("Med 0.5")
no_beef <- c("VEG", "Chicken diet")
unknown_beef <- c("Blue","Red","Green", "Yellow", "baseline", "BL", "C")

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
# meta_df$TREATMENT[meta_df$TREATMENT %in% control_treatments] <- "tech_control"
meta_df <- meta_df[!(meta_df$TREATMENT %in% control_treatments),]

##### Improve readability #####
meta_df$TREATMENT[meta_df$TREATMENT == "B"] <- "Chicken"
meta_df$TREATMENT[meta_df$TREATMENT == "A"] <- "Beef"
# meta_df$TREATMENT[meta_df$TREATMENT == "C"] <- "Control"
drops <- c("SITE")
meta_df <- meta_df[ , !(names(meta_df) %in% drops)]
colnames(meta_df)[colnames(meta_df) == "CORRECTED_SITE"] = "SITE"

#### Create df for meat sums, starting with mb ####
agg_columns <- c("beef", "chicken", "pork", "turkey", "processed", "meat")
mb_meat_totals <- data.frame(matrix(ncol = 0,
                                    nrow = length(unique(mb_auto_protn_df$Intervention))))
# names(meat_totals) <- agg_columns
row.names(mb_meat_totals) <- unique(mb_auto_protn_df$Intervention)
for (clm in agg_columns){
  meat_sum <- aggregate(mb_auto_protn_df[,clm], by=list(mb_auto_protn_df$Intervention), sum)
  row.names(meat_sum) <- meat_sum[,1]
  meat_sum <- meat_sum["x"]
  names(meat_sum) <- c(clm)
  if (ncol(mb_meat_totals) == 0){
    mb_meat_totals <- meat_sum
  }else{
    mb_meat_totals <- cbind(mb_meat_totals,meat_sum)
  }
}

#each amount was for 9 days, so they were averaged by dividing by 9
mb_meat_totals <- mb_meat_totals/3

#next add USDA to meat totals
usda_meat_totals <- data.frame(matrix(ncol = 0,
                                      nrow = length(unique(usda_auto_protn_df$Intervention))))
for (clm in agg_columns){
  meat_sum <- aggregate(usda_auto_protn_df[,clm], by=list(usda_auto_protn_df$Intervention), sum)
  row.names(meat_sum) <- meat_sum[,1]
  meat_sum <- meat_sum["x"]
  names(meat_sum) <- c(clm)
  if (ncol(usda_meat_totals) == 0){
    usda_meat_totals <- meat_sum
  }else{
    usda_meat_totals <- cbind(usda_meat_totals,meat_sum)
  }
}
usda_meat_totals <- usda_meat_totals/7 #average for each of the seven days
meat_totals <- rbind(mb_meat_totals, usda_meat_totals)

#### Add Purdue data ####
meat_totals["MED",] <- c(168,0,0,0,0,0)
meat_totals["VEG",] <- c(0,0,0,0,0,0)

#### Create new df with same rows as meta_d and add proteins of each participant####
meat_rows <- data.frame(matrix(ncol = ncol(meat_totals),
                               nrow = nrow(meta_df)))
names(meat_rows) <- names(meat_totals)
mb_ids <- c()
for (i in seq_along(1:nrow(meta_df))){
  id <- meta_df$CLIENT_SAMPLE_ID[i]
  site <- meta_df$SITE[i]
  treat <- meta_df$TREATMENT[i]
  print(i)
  print(paste(id, site))
  if (site %in% c("USDA-MED", "PSU-MED")){
    # "Med 2.5"      "Med 5.5"      "Med 0.5"      "AAD"          "Control"
    if (treat == "Med 0.5"){
      meat_rows[i,] <- usda_meat_totals["MED 0.5oz BEEF",]
    }
    if (treat == "Med 2.5"){
      meat_rows[i,] <- usda_meat_totals["MED 2.5oz BEEF",]
    }
    if (treat == "Med 5.5"){
      meat_rows[i,] <- usda_meat_totals["MED 5.5oz BEEF",]
    }
    if (treat == "AAD"){
      print("added AAD")
      meat_rows[i,] <- usda_meat_totals["TYPICAL AMERICAN",]
    }
    if (treat == "Control"){
      meat_rows[i,] <- c(NA,NA,NA,NA,NA,NA)
    }
  }
  if (site == "USDA-MAP" ){
    meat_rows[i,] <- rep(NA,6)
  }
  if (site == "MB/IIT"){
    mb_id <- mb_map[mb_map$Randomization == id, "Screen"]
    mb_ids <- c(mb_ids, mb_id)
    mb_treat <- 
    mb_pro_id <- paste0("SC_", formatC(mb_id, width=3, flag="0"), "_", treat)
    if (mb_pro_id %in% row.names(mb_meat_totals)){
      meat_rows[i,] <- mb_meat_totals[mb_pro_id,]
      print("labeled mb from esha")
    }else{
      if (treat == "Chicken"){
        meat_rows[i,] <- c(0,184,0,0,0,0)
      }else{
        if (treat == "Beef"){
          meat_rows[i,] <- c(184,0,0,0,0,0)
        }
      }
    }
  }
  if (site == "Purdue"){
    if (treat == "MED") meat_rows[i,] <- c(168,0,0,0,0,0)
    if (treat == "VEG") meat_rows[i,] <- c(0,0,0,0,0,0)
  }
}

meta_df <- cbind(meta_df, meat_rows)
new_rows <- data.frame(matrix(ncol = ncol(meat_totals),
                              nrow = nrow(meta_df)))
new_rows <- data.frame(lapply(new_rows, as.character))
names(new_rows) <- names(meat_totals)

new_rows[meat_rows >= 70] <- "high"
new_rows[meat_rows <= 0] <- "no"
new_rows[meat_rows < 70 & meat_rows > 0] <- "low"

colnames(new_rows) <- paste(colnames(meat_rows), "levels", sep = "_")

meta_df <- cbind(meta_df, new_rows)

meta_df <- meta_df[meta_df$SITE == "MB/IIT",]
meta_df <- meta_df[c("CLIENT_IDENTIFIER","CLIENT_SAMPLE_ID","CLIENT_SAMPLE_NUMBER",
                     "SITE","TIMEPOINT","TREATMENT")]
write.csv(meta_df, file = file.path("data", "mapping", "rf_only_MBIIT_auto_protn_metadata.csv"),
          row.names = FALSE)
#### Save output ####
write.csv(meat_totals, file = file.path(nut_dir, "auto_protn_meat_totals_by_treatment.csv"))

write.csv(meta_df, file = file.path("data", "mapping", "auto_protn_metadata.csv"),
          row.names = FALSE)

meta_df <- meta_df[! is.na(meta_df$beef),]

write.csv(meta_df, file = file.path("data", "mapping", "noMap_auto_protn_metadata.csv"),
          row.names = FALSE)

# Remove LCMS technical data for testing in random forest
# meta_df <- meta_df[c("PARENT_SAMPLE_NAME", "SITE","TIMEPOINT","TREATMENT", agg_columns, names(new_rows))]
meta_df <- meta_df[c("PARENT_SAMPLE_NAME", "SITE","TIMEPOINT","TREATMENT", agg_columns)]

# meta_df <- meta_df[c("PARENT_SAMPLE_NAME", agg_columns)]
write.csv(meta_df, file = file.path("data", "mapping", "rf_noMap_auto_protn_metadata.csv"),
          row.names = FALSE)

print("End R script.")

