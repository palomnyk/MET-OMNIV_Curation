# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for normalizing the meats to the same units.
# The data came to us in the following forms:
# •PSU: Grams per 2000 calorie daily requirement
# •USDA: Grams per 2000 calorie daily requirement
# •MB/IIT: Grams eaten
# •Purdue: Grams eaten
# Need to convert PSU and USDA to grams.
# Convert all to grams per kg bodyweight.

rm(list = ls()) #clear workspace

print(paste("Working in", getwd()))

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("networkD3", quietly = TRUE))  BiocManager::install("networkD3")
library("networkD3")
if (!requireNamespace("optparse", quietly = TRUE)) BiocManager::install("optparse")
library("optparse")

print("Loaded dependencies")
source(file.path("scripts","data_org", "data_org_func.R"))

#### Functions ####

#### Parse command line arguments ####
option_list <- list(
  optparse::make_option(c("-m", "--metblmcs_lev"), type="character", 
                        default="chem",
                        help="pathway_level, keyword to help find right random forest scores"),
  optparse::make_option(c("-i", "--in_dir"), type="character", 
                        default=file.path("output", "no_map_auto_protein", "tables"), 
                        help="dir with input 'scores' files"),
  optparse::make_option(c("-o", "--out_suffix"), type="character",
                        default = "site_comb.csv",
                        help="Suffix of output csv."),
  optparse::make_option(c("-u", "--out_dir"), type="character",
                        default = file.path("output", "ml_eval"),
                        help="Path of output directory."),
  optparse::make_option(c("-d", "--data_pattern"), type="character", 
                        default="demo-log-filt_all_bat_norm_imput",
                        # default="rf_demographics_meats",
                        help="pattern to separate score files if there are more than one group in the dir"),
  optparse::make_option(c("-p", "--accuracy_pattern"), type="character", 
                        default="shap_feat_imp", 
                        help="pattern to separate score files if there are more than one group in the dir")
);
opt_parser <- optparse::OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

print("Commandline arguments:")
print(opt)

#### Establish directory layout and other constants ####
output_dir <- file.path(opt$out_dir)
nut_dir <- file.path("data", "diet", "nutrition_data")

meta_df <- read.csv(file = file.path("data", "mapping", "noMap-meats.csv"),
                    check.names = FALSE)
usda_energy_weight <- as.data.frame(readxl::read_excel(
  file.path("data","diet","med","Body weight and energy intake data USDA HS54 and HS68.xlsx")),
  check.names=FALSE)
psu_energy <- read.csv(file.path("data","diet","med", "MED kcal level_PSU_May 25 2023_ay_wide.csv"),
                       check.names=FALSE)
PSU_weight <- as.data.frame(readxl::read_excel(
  file.path("data","diet","med","PSU_Weight.xlsx")),
  check.names=FALSE, trim_ws = TRUE)
mb_map <- as.data.frame(readxl::read_excel(file.path("data","mapping","mb", "MB-2112_Screen and Rand.xlsx")),
                        check.names=FALSE)
psu_energy <- read.csv(file.path("data","diet","med", "MED kcal level_PSU_May 25 2023_ay_wide.csv"),
                       check.names=FALSE)
PSU_weight <- as.data.frame(readxl::read_excel(
  file.path("data","diet","med","PSU_Weight.xlsx")),
  check.names=FALSE, trim_ws = TRUE)
purdue_meta <- openxlsx::read.xlsx(file.path("data","mapping", "purdue", "AY_sample_codebook 9.12.2024 Campbell data.xlsx"),
                                   sheet = "Subject Characteristics",
                                   sep.names = " ")
purdue_meta$short_id <- sapply(strsplit(purdue_meta$`Participant ID`, "-"), "[", 3)#take 3rd part of split
demo_data <- read.csv("data/mapping/noMap_demo.csv", check.names = FALSE,
                      row.names = "PARENT_SAMPLE_NAME")
correct_sites <- c("PSU-MED","MB/IIT","Purdue","USDA-MAP","USDA-MED")
agg_columns <- c("beef", "chicken", "pork", "turkey", "processed", "meat")

#### Add columns to fill in to meta_df ####
for (ac in agg_columns){
  meta_df[,paste0(ac,"_g")] <- meta_df[,ac]
  meta_df[,paste0(ac,"_g_per_kg_bw")] <- meta_df[,ac]
}

##### Cycle through participants and convert meats to absolute values and g/kg #####
missing_data <- c()
missing_ids <- c()

for (i in seq_along(1:nrow(meta_df))){
  id <- meta_df$CLIENT_SAMPLE_ID[i]
  site <- meta_df$SITE[i]
  treat <- meta_df$TREATMENT[i]
  timep <- meta_df$TIMEPOINT[i]
  chron_tp <- meta_df$CHRONOLOGICAL_TIMEPOINT[i]
  print(paste(id, site, timep, chron_tp))
  if (site %in% c("USDA-MED")){
    chron_tp <- chron_tp - 1
    my_energy <- usda_energy_weight[usda_energy_weight$SUBJCODE == id &
                                      usda_energy_weight$Period == chron_tp,
                                    "Calorie Level (kcal/d)"]
    my_grams <- meta_df[i, agg_columns] * my_energy/2000
    meta_df[i ,paste0(agg_columns,"_g")] <- my_grams
    my_bw <- usda_energy_weight[usda_energy_weight$SUBJCODE == id &
                                  usda_energy_weight$Period == chron_tp,
                                "Weight (kg)"]
    my_g_per_kg_bw <- my_grams/as.numeric(my_bw)
    meta_df[i ,paste0(agg_columns,"_g_per_kg_bw")] <- my_g_per_kg_bw
  }
  if (site %in% c("PSU-MED")){
    my_energy <- psu_energy[psu_energy$ID == paste0("MED", id),treat]
    my_grams <- meta_df[i, agg_columns] * my_energy/2000
    meta_df[i ,paste0(agg_columns,"_g")] <- my_grams
    my_bw <- PSU_weight[PSU_weight$ID == id &
                          PSU_weight$PERIOD == chron_tp - 1,
                        "WT_kg"]
    my_g_per_kg_bw <- my_grams/as.numeric(my_bw)
    meta_df[i ,paste0(agg_columns,"_g_per_kg_bw")] <- my_g_per_kg_bw
  }
  if (site == "USDA-MAP" ){
    meta_df[i ,agg_columns] <- rep(NA,6)
    meta_df[i ,paste0(agg_columns,"_g")] <- rep(NA,6)
    meta_df[i ,paste0(agg_columns,"_g_per_kg_bw")] <- rep(NA,6)
  }
  if (site == "MB/IIT"){
    mb_screen <- mb_map[mb_map$Randomization == id, "Screen"]
    mb_rand <- mb_map[mb_map$Randomization == id, "Randomization"]
    meta_df$mb_screen[i] <- mb_screen
    my_table <- read.csv(file = file.path("data","CVD","raw","mb",
                                          paste0("2112 V",chron_tp+1, ".csv")),
                         check.names = FALSE)
    
    my_bw <- my_table[my_table$Screen == mb_screen, "Weight"] * 0.45359237#convert to kg
    print(paste("MB weight:", my_bw))
    
    my_g_per_kg_bw <- meta_df[i, agg_columns]/as.numeric(my_bw)
    meta_df[i ,paste0(agg_columns,"_g_per_kg_bw")] <- my_g_per_kg_bw
  }
  if (site == "Purdue"){
    # asfddsf
    if (id %in% purdue_meta$short_id){
      my_bw <- purdue_meta[purdue_meta$short_id == id, "Wt (kg)"]
      my_g_per_kg_bw <- meta_df[i, agg_columns]/as.numeric(my_bw)
      meta_df[i ,paste0(agg_columns,"_g_per_kg_bw")] <- my_g_per_kg_bw
    }else{
      missing <- paste("meta_row:",i, "|site:", site,"|id:", id, "|treatment:", treat)
      missing_data <- c(missing_data, paste("site:", site,"| id:", id))
      missing_ids <- c(missing_ids, id)
      meta_df[i ,paste0(agg_columns,"_g_per_kg_bw")] <- rep(NA,6)
    }
  }
}

print(paste("Missing ids:", paste(unique(missing_data), collapse = ",")))

row.names(meta_df) <- meta_df$PARENT_SAMPLE_NAME

#### Save output ####
write.csv(meta_df,
          file = file.path(nut_dir, "all_sites-meats_normailize_full_df.csv"),
          row.names = FALSE)

for (site in unique(meta_df$SITE)){
  clean_site <- gsub("/", "_", site)
  clean_site <- gsub("-", "_", clean_site)
  sub_df <- meta_df[meta_df$SITE == site,]
  
  fname <- paste0(clean_site, "-", "meats_g.csv")
  write.csv(sub_df, file = file.path("data", "mapping", fname),
            row.names = FALSE)
  
  # Remove LCMS technical data for testing in random forest
  # sub_df <- sub_df[c("PARENT_SAMPLE_NAME", "SITE", "TREATMENT", agg_columns)]
  sub_df1 <- sub_df[c("PARENT_SAMPLE_NAME", paste0(agg_columns,"_g"))]
  fname <- paste0(clean_site, "-", "rf_meats_g.csv")
  write.csv(sub_df1, file = file.path("data", "mapping", fname),
            row.names = FALSE)
  
  # Add demo data to meats
  sub_df1 <- merge(sub_df1, demo_data, all = FALSE, by = 0)
  sub_df1 <- within(sub_df1, rm("Row.names"))
  fname <- paste0(clean_site, "-", "rf_demographics_meats_g.csv")
  write.csv(sub_df1, file = file.path("data", "mapping", fname),
            row.names = FALSE)
  
  # Remove LCMS technical data for testing in random forest
  # sub_df <- sub_df[c("PARENT_SAMPLE_NAME", "SITE", "TREATMENT", agg_columns)]
  sub_df2 <- sub_df[c("PARENT_SAMPLE_NAME", paste0(agg_columns,"_g_per_kg_bw"))]
  fname <- paste0(clean_site, "-", "rf_meats_g_per_kg_bw.csv")
  write.csv(sub_df2, file = file.path("data", "mapping", fname),
            row.names = FALSE)
  
  # Add demo data to meats
  sub_df2 <- merge(sub_df2, demo_data, all = FALSE, by = 0)
  sub_df2 <- within(sub_df2, rm("Row.names"))
  fname <- paste0(clean_site, "-", "rf_demographics_meats_g_per_kg_bw.csv")
  write.csv(sub_df2, file = file.path("data", "mapping", fname),
            row.names = FALSE)
}

meta_df <- meta_df[! is.na(meta_df$beef),]
# write.csv(meta_df, file = file.path("data", "mapping", "noMap-meats_g.csv"),
#           row.names = FALSE)
# 
# # Add demo data to meats
# sub_df <- sub_df[c("PARENT_SAMPLE_NAME", agg_columns)]
# sub_df <- merge(sub_df, demo_data, all = FALSE, by = 0)
# sub_df <- within(sub_df, rm("Row.names"))
# fname <- paste0("noMap", "-", "rf_demographics_meats_g.csv")
# write.csv(sub_df, file = file.path("data", "mapping", fname),
#           row.names = FALSE)
# 
# 
# #### Make non-mb-non-map dataset ####
# sub_df <- meta_df[meta_df$SITE != "MB/IIT",]
# write.csv(sub_df, file = file.path("data", "mapping", "noMB_noMap-meats_g.csv"),
#           row.names = FALSE)
# sub_df <- sub_df[c("PARENT_SAMPLE_NAME", agg_columns)]
# write.csv(sub_df, file = file.path("data", "mapping", "noMB_noMap-rf_meats_g.csv"),
#           row.names = FALSE)
# 
# # Add demo data to meats
# sub_df <- merge(sub_df, demo_data, all = FALSE, by = 0)
# sub_df <- within(sub_df, rm("Row.names"))
# fname <- paste0("noMB_noMap", "-", "rf_demographics_meats_g.csv")
# write.csv(sub_df, file = file.path("data", "mapping", fname),
#           row.names = FALSE)
# 
# Remove LCMS technical data for testing in random forest
# meta_df <- meta_df[c("PARENT_SAMPLE_NAME", "SITE","TIMEPOINT","TREATMENT", agg_columns, names(new_rows))]
sub_df <- meta_df[c("PARENT_SAMPLE_NAME", paste0(agg_columns,"_g"))]
write.csv(sub_df, file = file.path("data", "mapping", "noMap-rf_meats_g.csv"),
          row.names = FALSE)
sub_df <- meta_df[c("PARENT_SAMPLE_NAME", paste0(agg_columns,"_g_per_kg_bw"))]
write.csv(sub_df, file = file.path("data", "mapping", "noMap-rf_meats_g_per_kg_bw.csv"),
          row.names = FALSE)


# # Add demo data to meats
# sub_df <- merge(meta_df, demo_data, all = FALSE, by = 0)
# sub_df <- within(sub_df, rm("Row.names"))
# fname <- paste0("noMap", "-", "rf_demographics_meats_g.csv")
# write.csv(sub_df, file = file.path("data", "mapping", fname),
#           row.names = FALSE)

print("End R script.")



