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
if (!requireNamespace("combinat", quietly = TRUE))  BiocManager::install("combinat")
library("combinat")
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
norm_suffixes <- c("_g", "_g_per_kg_bw", "_g_per_bmi")
clean_sites <- c()

#### Add columns to fill in to meta_df ####
for (ac in agg_columns){
  meta_df[,paste0(ac,"_g")] <- meta_df[,ac]
  meta_df[,paste0(ac,"_g_per_kg_bw")] <- meta_df[,ac]
  meta_df[,paste0(ac,"_g_per_bmi")] <- meta_df[,ac]
}

##### Cycle through participants and convert meats to absolute values and g/kg #####
missing_data <- c()
missing_ids <- c()
body_weight <- vector(mode = "numeric", length = nrow(meta_df))
daily_energy <- vector(mode = "numeric", length = nrow(meta_df))
bmi <- vector(mode = "numeric", length = nrow(meta_df))
sex <- vector(mode = "numeric", length = nrow(meta_df))
age <- vector(mode = "numeric", length = nrow(meta_df))

for (i in seq_along(1:nrow(meta_df))){
  id <- meta_df$CLIENT_SAMPLE_ID[i]
  psn <- meta_df$PARENT_SAMPLE_NAME[i]
  site <- meta_df$SITE[i]
  treat <- meta_df$TREATMENT[i]
  timep <- meta_df$TIMEPOINT[i]
  chron_tp <- meta_df$CHRONOLOGICAL_TIMEPOINT[i]
  my_bmi <- demo_data[psn, "bmi"]
  my_sex <- demo_data[psn, "sex"]
  my_age <- demo_data[psn, "age"]
  print(paste(id, site, timep, chron_tp))
  my_energy <- NA
  my_weight <- NA
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

	  my_g_per_bmi <- my_grams/as.numeric(my_bmi)
    meta_df[i ,paste0(agg_columns,"_g_per_bmi")] <- my_g_per_bmi
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
    
    my_g_per_bmi <- my_grams/as.numeric(my_bmi)
    meta_df[i ,paste0(agg_columns,"_g_per_bmi")] <- my_g_per_bmi
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
    
    my_g_per_bmi <- my_grams/as.numeric(my_bmi)
    meta_df[i ,paste0(agg_columns,"_g_per_bmi")] <- my_g_per_bmi
  }
  if (site == "Purdue"){
    # asfddsf
    if (id %in% purdue_meta$short_id){
      my_bw <- purdue_meta[purdue_meta$short_id == id, "Wt (kg)"]
      my_g_per_kg_bw <- meta_df[i, agg_columns]/as.numeric(my_bw)
      meta_df[i ,paste0(agg_columns,"_g_per_kg_bw")] <- my_g_per_kg_bw
      my_g_per_bmi <- my_grams/as.numeric(my_bmi)
      meta_df[i ,paste0(agg_columns,"_g_per_bmi")] <- my_g_per_bmi
    }else{
      missing <- paste("meta_row:",i, "|site:", site,"|id:", id, "|treatment:", treat)
      missing_data <- c(missing_data, paste("site:", site,"| id:", id))
      missing_ids <- c(missing_ids, id)
      meta_df[i ,paste0(agg_columns,"_g_per_kg_bw")] <- rep(NA,6)
      my_bw <- NA
      my_bmi <- NA
    }
  }
  body_weight[i] <- my_bw[1]
  daily_energy[i] <- my_energy
  bmi[i] <- my_bmi
  sex[i] <- my_sex
  age[i] <- my_age
}

print(paste("Missing ids:", paste(unique(missing_data), collapse = ",")))

row.names(meta_df) <- meta_df$PARENT_SAMPLE_NAME
meta_df$bodyweight <- body_weight
meta_df$daily_energy <- daily_energy
meta_df$sex <- sex
meta_df$age <- age
meta_df$bmi <- bmi

#### Log rf columns ####
# for (ac in agg_columns){
#   meta_df[,paste0(ac,"_g")] <- log(meta_df[,paste0(ac,"_g")] + 1)
#   meta_df[,paste0(ac,"_g_per_kg_bw")] <- log(meta_df[,paste0(ac,"_g_per_kg_bw")] + 1)
#   meta_df[,paste0(ac,"_g_per_bmi")] <- log(meta_df[,paste0(ac,"_g_per_bmi")] + 1)
# }

#### Remove outliers ####
for (ac in agg_columns){
  for (ns in norm_suffixes){
    my_col <- meta_df[,paste0(ac,ns)]
    my_Q3 <- quantile(my_col, .75, na.rm = TRUE)
    my_IQR <- IQR(my_col, na.rm = TRUE)
    my_col[my_col > my_Q3 + 1.5*my_IQR] = NA
    meta_df[paste0(ac,"_rmOut",ns)] <- my_col
    print(identical(my_col, meta_df[,paste0(ac,ns)]))
  }
}
all_suffixes <- c(norm_suffixes, paste0("_rmOut",norm_suffixes))

#### Save output ####
write.csv(meta_df,
          file = file.path(nut_dir, "all_sites-meats_normalize_full_df.csv"),
          row.names = FALSE)

##### Data for Dr. Jonathan Shao #####
meta_demo <- meta_df[c("SITE", "CLIENT_IDENTIFIER", "TREATMENT", "beef", "beef_g", "beef_g_per_kg_bw", "bodyweight", "daily_energy", "bmi", "sex", "age")]
# "chicken", "pork", "turkey", "processed","meat"
# "beef_g", "beef_g_per_kg_bw", "age","bmi", "age","sex"
# "chicken_g", "chicken_g_per_kg_bw", "pork_g","pork_g_per_kg_bw",
# "turkey_g","turkey_g_per_kg_bw","processed_g","processed_g_per_kg_bw",
# "meat_g", "meat_g_per_kg_bw",
set.seed((777))
meta_demo <- meta_demo[sample(nrow(meta_demo), 200), ]
write.csv(meta_demo,
          file = file.path("output", "ml_eval", "all_sites-beef_demo_random_sample.csv"),
          row.names = FALSE)
##### End of: Data for Dr. Shao #####

##### Save single site normalized meats #####
for (site in unique(meta_df$SITE)){
  clean_site <- gsub("/", "_", site)
  clean_site <- gsub("-", "_", clean_site)
  sub_df <- meta_df[meta_df$SITE == site,]
  
  for (ns in all_suffixes){
  fname <- paste0(clean_site, "-", "meats", ns, ".csv")
  write.csv(sub_df, file = file.path("data", "mapping", fname),
            row.names = FALSE)
  print(fname)
  # Remove LCMS technical data for testing in random forest
  # sub_df <- sub_df[c("PARENT_SAMPLE_NAME", "SITE", "TREATMENT", agg_columns)]
  sub_df1 <- sub_df[c("PARENT_SAMPLE_NAME", paste0(agg_columns,ns))]
  fname <- paste0(clean_site, "-", "rf_meats",ns,".csv")
  write.csv(sub_df1, file = file.path("data", "mapping", fname),
            row.names = FALSE)
  print(fname)
  # Add demo data to meats
  sub_df1 <- merge(sub_df1, demo_data, all = FALSE, by = 0)
  sub_df1 <- within(sub_df1, rm("Row.names"))
  fname <- paste0(clean_site, "-", "rf_demographics_meats",ns,".csv")
  write.csv(sub_df1, file = file.path("data", "mapping", fname),
            row.names = FALSE)
      print(fname)
  }
}

##### Save all sites together normalized meats #####
meta_df <- meta_df[! is.na(meta_df$beef),]

for (ns in all_suffixes){
  # Remove LCMS technical data for testing in random forest
  # meta_df <- meta_df[c("PARENT_SAMPLE_NAME", "SITE","TIMEPOINT","TREATMENT", agg_columns, names(new_rows))]
  sub_df1 <-meta_df[c("PARENT_SAMPLE_NAME", paste0(agg_columns,ns))]
  write.csv(sub_df1, file = file.path("data", "mapping", paste0("noMap-rf_meats", ns, ".csv")),
            row.names = FALSE)
  # # Add demo data to meats
  # sub_df <- merge(sub_df, demo_data, all = FALSE, by = 0)
  # sub_df <- within(sub_df, rm("Row.names"))
  # fname <- paste0("noMB_noMap", "-", "rf_demographics_meats_g.csv")
  # write.csv(sub_df, file = file.path("data", "mapping", fname),
  #           row.names = FALSE)
}

###### Add demo data to meats ######
# sub_df1 <- meta_df[c("PARENT_SAMPLE_NAME", agg_columns)]
# sub_df1 <- merge(sub_df1, demo_data, all = FALSE, by = 0)
# sub_df1 <- within(sub_df1, rm("Row.names"))
# fname <- paste0("noMap", "-", "rf_demographics_meats_g_per_kg_bw.csv")
# write.csv(sub_df1, file = file.path("data", "mapping", fname),
#           row.names = FALSE)
# 
# sub_df1 <- sub_df1[c("PARENT_SAMPLE_NAME", agg_columns)]
# sub_df1 <- merge(sub_df1, demo_data, all = FALSE, by = 0)
# sub_df1 <- within(sub_df1, rm("Row.names"))
# fname <- paste0("noMap", "-", "rf_demographics_meats_g.csv")
# write.csv(sub_df1, file = file.path("data", "mapping", fname),
#           row.names = FALSE)

###### Make all 2 site datasets ######
two_sites <- combinat::combn(unique(meta_df$SITE), 2, simplify = TRUE)

for (st in seq_along(1:ncol(two_sites))){
  my_sites <- two_sites[,st]
  sub_df <- meta_df[meta_df$SITE %in% my_sites,]
  
  for (ns in all_suffixes){
    # Remove LCMS technical data for testing in random forest
    # sub_df <- sub_df[c("PARENT_SAMPLE_NAME", "SITE", "TREATMENT", agg_columns)]
    sub_df1 <- sub_df[c("PARENT_SAMPLE_NAME", paste0(agg_columns, ns))]
    clean_site <- paste(my_sites, collapse = "_")
    clean_site <- gsub("/", "_", clean_site)
    clean_site <- gsub("-", "_", clean_site)
    clean_sites <- c(clean_sites, clean_site)
    
    fname <- paste0(clean_site, "-", "rf_meats", ns, ".csv")
    write.csv(sub_df1, file = file.path("data", "mapping", fname),
              row.names = FALSE)
    print(fname)
  }
}
###### Make all 3 site datasets ######
three_sites <- combn(unique(meta_df$SITE), 3, simplify = TRUE)
for (st in seq_along(1:ncol(three_sites))){
  my_sites <- three_sites[,st]
  sub_df <- meta_df[meta_df$SITE %in% my_sites,]
  
  for (ns in all_suffixes){
    # Remove LCMS technical data for testing in random forest
    # sub_df <- sub_df[c("PARENT_SAMPLE_NAME", "SITE", "TREATMENT", agg_columns)]
    sub_df1 <- sub_df[c("PARENT_SAMPLE_NAME", paste0(agg_columns,ns))]
    clean_site <- paste(my_sites, collapse = "_")
    clean_site <- gsub("/", "_", clean_site)
    clean_site <- gsub("-", "_", clean_site)
    clean_sites <- c(clean_sites, clean_site)
    
    fname <- paste0(clean_site, "-", "rf_meats", ns, ".csv")
    write.csv(sub_df1, file = file.path("data", "mapping", fname),
              row.names = FALSE)
    print(fname)
  }
}

print("End R script.")




