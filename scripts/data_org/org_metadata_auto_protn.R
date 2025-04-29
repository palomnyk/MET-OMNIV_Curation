# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for organizing auto and hand labeled protein categories
# Notes:
# Dropped "SITE" column and change "CORRECTED_SITE" to "SITE"

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
mb_auto_protn_df <- read.csv(file = file.path(nut_dir ,"mb-esha_meats_FPED_vals.tsv"),
                             sep = "\t", check.names = FALSE)
mb_map <- as.data.frame(readxl::read_excel(file.path("data","mapping","mb", "MB-2112_Screen and Rand.xlsx")),
                        check.names=FALSE)
# usda_auto_protn_df <- read.csv(file = file.path(nut_dir ,"esha_combined_meats_HEI_vals.tsv"),
#                                sep = "\t", check.names = FALSE)
usda_auto_protn_df <- read.csv(file = file.path(nut_dir, "esha_combined_meats_HEI_vals28Sep2023.tsv"),
                               sep = "\t", check.names = FALSE)
demo_data <- read.csv("data/mapping/noMap_demo.csv", check.names = FALSE,
                      row.names = "PARENT_SAMPLE_NAME")
control_treatments <- c("Med 100","Purple", "Orange","Med 200","USUAL", "C")
correct_sites <- c("PSU-MED","MB/IIT","Purdue","USDA-MAP","USDA-MED")

unknown_beef <- c("Blue","Red","Green", "Yellow", "baseline", "BL")

# Preserve orginal treatement labels for troubhshooting:
meta_df$ORIG_TREAT <- meta_df$TREATMENT

#### Fix some inconsistencies ####
meta_df$controls <- meta_df$TREATMENT %in% control_treatments
meta_df$TREATMENT[meta_df$TREATMENT == "MED 0.5"] <- "Med 0.5"
meta_df$TREATMENT[meta_df$TREATMENT == "MED 2.5"] <- "Med 2.5"
meta_df$TREATMENT[meta_df$TREATMENT == "MED 5.5"] <- "Med 5.5"
meta_df$TREATMENT[meta_df$TREATMENT == "post "] <- "post"

#### Create "control" columns ####
# meta_df$TREATMENT[meta_df$TREATMENT %in% control_treatments] <- "tech_control"
meta_df <- meta_df[!(meta_df$TREATMENT %in% control_treatments),]
meta_df <- meta_df[!(meta_df$TREATMENT %in% unknown_beef),]

##### Improve readability #####
drops <- c("SITE")
meta_df <- meta_df[ , !(names(meta_df) %in% drops)]
colnames(meta_df)[colnames(meta_df) == "CORRECTED_SITE"] = "SITE"

meta_df$TREATMENT[meta_df$TREATMENT == "A"] <- "Chicken"
meta_df$TREATMENT[meta_df$TREATMENT == "B"] <- "Beef"

#### Add column to show which baseline is first for each MB participant ####
mb_rand_labels <- meta_df[meta_df$SITE == "MB/IIT", "CLIENT_SAMPLE_ID"]
mb_parents <- meta_df[meta_df$SITE == "MB/IIT", "PARENT_SAMPLE_NAME"]
documented_usuals <- c()#parents of the baseline before the mb_map$`Study Product First`
for (parent in meta_df$PARENT_SAMPLE_NAME){
  if (parent %in% mb_parents){
    mb_rand_label <- meta_df[meta_df$PARENT_SAMPLE_NAME == parent, "CLIENT_SAMPLE_ID"]
    timepoint <- meta_df[meta_df$PARENT_SAMPLE_NAME == parent, "TIMEPOINT"]
    treatment <- meta_df[meta_df$PARENT_SAMPLE_NAME == parent, "TREATMENT"]

    first_treat <- mb_map[mb_map$Randomization == mb_rand_label, "Study Product First"]
    if (timepoint == "baseline" & treatment == first_treat){
      documented_usuals <- c(documented_usuals, "TRUE")
    }else{
      if(timepoint == "post"){
        documented_usuals <- c(documented_usuals, "POST")
      }else{
        documented_usuals <- c(documented_usuals, "FALSE")
      }
    }
  }else{
    documented_usuals <- c(documented_usuals, "NOT_MB")
  }
}
meta_df$documented_usual <- documented_usuals

# Add USUAL diet to MB treatment
meta_df$TREATMENT[meta_df$SITE == "MB/IIT" & meta_df$TIMEPOINT == "baseline"] <- "Usual"
# Remove usual diets that not documented
# meta_df <- meta_df[!meta_df$documented_usual == "FALSE",]

#### Create df for meat sums, starting with mb ####
agg_columns <- c("beef", "chicken", "pork", "turkey", "processed", "meat")
mb_meat_totals <- data.frame(matrix(ncol = 0,
                                    nrow = length(unique(mb_auto_protn_df$Intervention))))
# names(meat_totals) <- agg_columns

#### Aggregate each mb intervention ####
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

#each amount was for 3 days, so they were averaged by dividing by 3
mb_meat_totals <- mb_meat_totals/3

#### USDA meat totals ####
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

#### Create new df with same rows as meta_d and add proteins of each participant####
meat_rows <- data.frame(matrix(ncol = ncol(meat_totals),
                               nrow = nrow(meta_df)))
names(meat_rows) <- names(meat_totals)

meta_df$mb_screen <- rep(NA, nrow(meta_df))
meta_df$mb_rand <- rep(NA, nrow(meta_df))
meta_df$mb_intervention <- rep(NA, nrow(meta_df))
for (i in seq_along(1:nrow(meta_df))){
  id <- meta_df$CLIENT_SAMPLE_ID[i]
  site <- meta_df$SITE[i]
  treat <- meta_df$TREATMENT[i]
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
      stop("Found treatement of Control in USDA columns, it shouldn't be there")
    }
  }
  if (site == "USDA-MAP" ){
    meat_rows[i,] <- rep(NA,6)
  }
  if (site == "MB/IIT"){
    mb_id <- mb_map[mb_map$Randomization == id, "Screen"]
    mb_rand <- mb_map[mb_map$Randomization == id, "Randomization"]
    meta_df$mb_screen[i] <- mb_id
    mb_pro_id <- paste0("SC_", formatC(mb_id, width=3, flag="0"), "_", treat)
    if (mb_pro_id %in% row.names(mb_meat_totals)){
      meat_rows[i,] <- mb_meat_totals[mb_pro_id,]
      meta_df$mb_intervention[i] <- mb_pro_id
      print("labeled mb from esha")
    }else{
      meta_df$mb_intervention[i] <- paste0("missing_", mb_pro_id)
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

#### Arbitrary "meat levels" for classifiers ####
new_rows <- data.frame(matrix(ncol = ncol(meat_totals),
                              nrow = nrow(meta_df)))
new_rows <- data.frame(lapply(new_rows, as.character))
names(new_rows) <- names(meat_totals)

new_rows[meat_rows >= 70] <- "high"
new_rows[meat_rows <= 0] <- "no"
new_rows[meat_rows < 70 & meat_rows > 0] <- "low"

colnames(new_rows) <- paste(colnames(meat_rows), "levels", sep = "_")

meta_df <- cbind(meta_df, new_rows)

# meta_df <- meta_df[meta_df$TREATMENT != "Usual",]
# meta_df <- meta_df[meta_df$SITE == "Purdue",]

row.names(meta_df) <- meta_df$PARENT_SAMPLE_NAME

#### Save output ####
meat_rows$PARENT_SAMPLE_NAME <- meta_df$PARENT_SAMPLE_NAME
write.csv(meat_rows,
          file = file.path(nut_dir, "meat_total_participant.csv"),
          row.names = FALSE)
write.csv(meat_totals,
          file = file.path(nut_dir, "meat_total_treatment.csv"),
          row.names = FALSE)
for (site in unique(meta_df$SITE)){
  clean_site <- gsub("/", "_", site)
  clean_site <- gsub("-", "_", clean_site)
  sub_df <- meta_df[meta_df$SITE == site,]
  
  fname <- paste0(clean_site, "-", "meats.csv")
  write.csv(sub_df, file = file.path("data", "mapping", fname),
            row.names = FALSE)
  
  # Remove LCMS technical data for testing in random forest
  # sub_df <- sub_df[c("PARENT_SAMPLE_NAME", "SITE", "TREATMENT", agg_columns)]
  sub_df <- sub_df[c("PARENT_SAMPLE_NAME", agg_columns)]
  fname <- paste0(clean_site, "-", "rf_meats.csv")
  write.csv(sub_df, file = file.path("data", "mapping", fname),
            row.names = FALSE)
  
  # Add demo data to meats
  sub_df <- merge(sub_df, demo_data, all = FALSE, by = 0)
  sub_df <- within(sub_df, rm("Row.names"))
  fname <- paste0(clean_site, "-", "rf_demographics_meats.csv")
  write.csv(sub_df, file = file.path("data", "mapping", fname),
            row.names = FALSE)
  
}

write.csv(meat_totals, file = file.path(nut_dir, "auto_protn_meat_totals_by_treatment.csv"))

write.csv(meta_df, file = file.path("data", "mapping", "all_sites-meats.csv"),
          row.names = FALSE)

meta_df <- meta_df[! is.na(meta_df$beef),]
write.csv(meta_df, file = file.path("data", "mapping", "noMap-meats.csv"),
          row.names = FALSE)

# Add demo data to meats
sub_df <- sub_df[c("PARENT_SAMPLE_NAME", agg_columns)]
sub_df <- merge(sub_df, demo_data, all = FALSE, by = 0)
sub_df <- within(sub_df, rm("Row.names"))
fname <- paste0("noMap", "-", "rf_demographics_meats.csv")
write.csv(sub_df, file = file.path("data", "mapping", fname),
          row.names = FALSE)


#### Make non-mb-non-map dataset ####
sub_df <- meta_df[meta_df$SITE != "MB/IIT",]
write.csv(sub_df, file = file.path("data", "mapping", "noMB_noMap-meats.csv"),
          row.names = FALSE)
sub_df <- sub_df[c("PARENT_SAMPLE_NAME", agg_columns)]
write.csv(sub_df, file = file.path("data", "mapping", "noMB_noMap-rf_meats.csv"),
          row.names = FALSE)

# Add demo data to meats
sub_df <- merge(sub_df, demo_data, all = FALSE, by = 0)
sub_df <- within(sub_df, rm("Row.names"))
fname <- paste0("noMB_noMap", "-", "rf_demographics_meats.csv")
write.csv(sub_df, file = file.path("data", "mapping", fname),
          row.names = FALSE)

# Remove LCMS technical data for testing in random forest
# meta_df <- meta_df[c("PARENT_SAMPLE_NAME", "SITE","TIMEPOINT","TREATMENT", agg_columns, names(new_rows))]
meta_df <- meta_df[c("PARENT_SAMPLE_NAME", agg_columns)]
write.csv(meta_df, file = file.path("data", "mapping", "noMap-rf_meats.csv"),
          row.names = FALSE)

# Add demo data to meats
sub_df <- merge(meta_df, demo_data, all = FALSE, by = 0)
sub_df <- within(sub_df, rm("Row.names"))
fname <- paste0("noMap", "-", "rf_demographics_meats.csv")
write.csv(sub_df, file = file.path("data", "mapping", fname),
          row.names = FALSE)

print("End R script.")

