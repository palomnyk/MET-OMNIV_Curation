# Author: Aaron Yerke (aaronyerke@gmail.com)

# Calculate HEI for MED and MAP studies. 
# And compare to the PIVOT table that Patrick Sullivan labeled.
# HEI_gb_cup	HEI_dairy_cp	HEI_total_pro_oz	HEI_SeaPlantPro_oz	HEI_refinedG_oz
# HEI_addedSug_tsp	HEI_total_veg_cup	HEI_total_fruit_cup	HEI_total_wholeFruit_cup
# HEI_total_wholeGrains_cup

# This information is in "data/diet/nutrition_data/esha_combined_meats_HEI_vals28Jun2025.xlsx"

# We need to add Fatty Acid, Sodium, and Saturated fat.
#
#   FATTYACID:
#     Formula: (Total Monounsaturated Fatty Acids + Total Polyunsaturated Fatty Acids)/Total Saturated Fatty Acids
#     
#   SODIUM: grams per kcal
#   Saturated FAT:
#     
#
# According to https://epi.grants.cancer.gov/hei/comparing.html, the 2015 HEI 
# and the 2020 HEI scores are the same for adults.
# 
# Datasets:
#   MAP and MED:
#   File: esha_combined_meats_HEI_vals.xlsx
# Column headers:
#   HEI_gb_cup	HEI_dairy_cp	HEI_total_pro_oz	HEI_SeaPlantPro_oz	HEI_refinedG_oz	HEI_addedSug_tsp	HEI_total_veg_cup	HEI_total_fruit_cup	HEI_total_wholeFruit_cup	HEI_total_wholeGrains_cup
# Need to calculate:
#   FATTYACID:
#     Formula: (Total Monounsaturated Fatty Acids + Total Polyunsaturated Fatty Acids)/Total Saturated Fatty Acids
#   SODIUM:
#   
#   Saturated FAT:

rm(list = ls()) #clear workspace

print(paste("Working in", getwd()))

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")

print("Loaded dependencies")

#### Establish directory layout and other constants ####

##### Loading in data #####
combined_esha <- read.csv(file = file.path("data","diet","nutrition_data","esha_combined_meats_HEI_vals28Jun2025.tsv"),
                          check.names = FALSE, sep = "\t")

Pats_MAP_HEI_scores <- data.frame(treatment = c("Typical American processed", "Typical American unprocessed",
                                          "High HEI processed", "High HEI unprocessed"),
                            HEI_score = c(49.45089496, 55.16964886, 86.00128074,
                                          90.00458208))
# Dataframe that shows the HEI category values calculated by Patrick Sulivan
Pats_high_HEI_unproc_cat_vals <- read.csv(file = file.path("data","unit_test","HEI","MAP_high_HEI_unprocessed.csv"),
                          check.names = FALSE)
# Note about the column names for Pats_high_HEI_unproc_cat_vals - "Total Points"
# is the total points for each category and "Total Points w minus" is the same for most
# categories, but a few need an extra step, where the total is subtracted from
# a maximum. Cutoff points is the number of points that doesn't exceed the
# maximum points.

intrvntn <- "HIgh HEI Further Processed"
intrv_diet <- combined_esha[combined_esha$`Intervention`==intrvntn,]
intrvntns <- unique(combined_esha$`Intervention`)

#### Calculate sums of HEI variables ####
##### HEI variables that we already have in the combined studies df #####
HEI_to_calc <- c("HEI_addedSug_tsp","HEI_dairy_cup",
                 "HEI_gb_cup","HEI_SeaPlantPro_oz",
                 "HEI_total_pro_oz","HEI_total_veg_cup",
                 "HEI_total_fruit_cup","HEI_refinedG_oz",
                 "HEI_total_wholeFruit_cup",
                 "HEI_total_wholeGrains_oz")
HEI_calc_df_names <- c("HEI_category","Totals","Daily Avg.","Max Points",
                       "Total Points","Cutoff points")
HEI_calc_df <- data.frame(matrix(nrow = nrow(Pats_high_HEI_unproc_cat_vals),
                                  ncol = length(HEI_calc_df_names)))
names(HEI_calc_df) <- HEI_calc_df_names
HEI_calc_df$HEI_category <- Pats_high_HEI_unproc_cat_vals$Our_name
HEI_calc_df$`Max Points`<- Pats_high_HEI_unproc_cat_vals[match(HEI_calc_df$HEI_category, Pats_high_HEI_unproc_cat_vals$Our_name), "Max Points"]
HEI_calc_df$Goal<- Pats_high_HEI_unproc_cat_vals[match(HEI_calc_df$HEI_category, Pats_high_HEI_unproc_cat_vals$Our_name), "Goal"]

big_table <- data.frame() #Empty placeholder for table of intervention results together
###### Data for debugging ######
HEI_scores <- c()
intrv_sum_gram <- c()
intrv_sum_sug_conv <- c()
intrv_count_sug_conv <- c()
intrv_sum_sug_HEI <- c()

#####For loop to build table of HEI scores #####
for(intrvntn in intrvntns){
  print(intrvntn)
  daily_total_energy <- sum(intrv_diet$Energy)/7
  intrv_diet <- combined_esha[combined_esha$Intervention == intrvntn, ]
  
  for (rw in 1:length(HEI_to_calc)) {
    HEI_cat <- HEI_calc_df$HEI_category[rw]
    HEI_calc_df$Totals[rw] <- sum(intrv_diet[,HEI_cat])
    HEI_calc_df$`Daily Avg.`[rw] <- HEI_calc_df$Totals[rw]/7
    print(HEI_cat)
    if (HEI_cat == "HEI_addedSug_tsp"){
      threshold <- daily_total_energy * 0.065 # Should be ≤ 6.5% energy
      HEI_calc_df$`Total Points`[rw] <- threshold/HEI_calc_df$`Daily Avg.`[rw] * HEI_calc_df$`Max Points`[rw]
      
      if (HEI_calc_df$`Total Points`[rw] > HEI_calc_df$`Max Points`[rw]) HEI_calc_df$`Cutoff points`[rw] <- HEI_calc_df$`Max Points`[rw]
      else HEI_calc_df$`Cutoff points` <- HEI_calc_df$`Total Points`[rw]
    }
    if (HEI_cat == "HEI_dairy_cup"){
      threshold <- 2.6 # Should be >= 2.6 cups
      HEI_calc_df$`Total Points`[rw] <- HEI_calc_df$`Daily Avg.`[rw]/threshold * HEI_calc_df$`Max Points`[rw]
      if (HEI_calc_df$`Total Points`[rw] > HEI_calc_df$`Max Points`[rw]) HEI_calc_df$`Cutoff points`[rw] <- HEI_calc_df$`Max Points`[rw]
      else HEI_calc_df$`Cutoff points`[rw] <- HEI_calc_df$`Total Points`[rw]
    }
    if (HEI_cat == "HEI_gb_cup"){
      threshold <- 0.4 # Should be ≥0.4 cups
      HEI_calc_df$`Total Points`[rw] <- HEI_calc_df$`Daily Avg.`[rw]/threshold * HEI_calc_df$`Max Points`[rw]
      if (HEI_calc_df$`Total Points`[rw] > HEI_calc_df$`Max Points`[rw]) HEI_calc_df$`Cutoff points`[rw] <- HEI_calc_df$`Max Points`[rw]
      else HEI_calc_df$`Cutoff points`[rw] <- HEI_calc_df$`Total Points`[rw]
    }
    if (HEI_cat == "HEI_SeaPlantPro_oz"){
      threshold <- 1.6 # Should be ≥1.6 oz.
      HEI_calc_df$`Total Points`[rw] <- HEI_calc_df$`Daily Avg.`[rw]/threshold * HEI_calc_df$`Max Points`[rw]
      if (HEI_calc_df$`Total Points`[rw] > HEI_calc_df$`Max Points`[rw]) HEI_calc_df$`Cutoff points`[rw] <- HEI_calc_df$`Max Points`[rw]
      else HEI_calc_df$`Cutoff points`[rw] <- HEI_calc_df$`Total Points`[rw]
    }
    if (HEI_cat == "HEI_total_pro_oz"){
      threshold <- 5 # Should be ≥5.0 oz.
      HEI_calc_df$`Total Points`[rw] <- HEI_calc_df$`Daily Avg.`[rw]/threshold * HEI_calc_df$`Max Points`[rw]
      if (HEI_calc_df$`Total Points`[rw] > HEI_calc_df$`Max Points`[rw]) HEI_calc_df$`Cutoff points`[rw] <- HEI_calc_df$`Max Points`[rw]
      else HEI_calc_df$`Cutoff points`[rw] <- HEI_calc_df$`Total Points`[rw]
    }
    if (HEI_cat == "HEI_total_veg_cup"){
      threshold <- 2.2 # Should be ≥ 2.2 cups
      HEI_calc_df$`Total Points`[rw] <- HEI_calc_df$`Daily Avg.`[rw]/threshold * HEI_calc_df$`Max Points`[rw]
      if (HEI_calc_df$`Total Points`[rw] > HEI_calc_df$`Max Points`[rw]) HEI_calc_df$`Cutoff points`[rw] <- HEI_calc_df$`Max Points`[rw]
      else HEI_calc_df$`Cutoff points`[rw] <- HEI_calc_df$`Total Points`[rw]
    }
    if (HEI_cat == "HEI_total_fruit_cup"){
      threshold <- 1.6 # Should be ≥1.6 cups
      HEI_calc_df$`Total Points`[rw] <- HEI_calc_df$`Daily Avg.`[rw]/threshold * HEI_calc_df$`Max Points`[rw]
      if (HEI_calc_df$`Total Points`[rw] > HEI_calc_df$`Max Points`[rw]) HEI_calc_df$`Cutoff points`[rw] <- HEI_calc_df$`Max Points`[rw]
      else HEI_calc_df$`Cutoff points`[rw] <- HEI_calc_df$`Total Points`[rw]
    }
    if (HEI_cat == "HEI_refinedG_oz"){
      threshold <- 3.6 # Should be ≤ 3.6 oz.
      HEI_calc_df$`Total Points`[rw] <- threshold/HEI_calc_df$`Daily Avg.`[rw] * HEI_calc_df$`Max Points`[rw]
      if (HEI_calc_df$`Total Points`[rw] > HEI_calc_df$`Max Points`[rw]) HEI_calc_df$`Cutoff points`[rw] <- HEI_calc_df$`Max Points`[rw]
      else HEI_calc_df$`Cutoff points`[rw] <- HEI_calc_df$`Total Points`[rw]
    }
    if (HEI_cat == "HEI_total_wholeFruit_cup"){
      threshold <- 0.8 # Should be ≥ 0.8 cups
      HEI_calc_df$`Total Points`[rw] <- HEI_calc_df$`Daily Avg.`[rw]/threshold * HEI_calc_df$`Max Points`[rw]
      if (HEI_calc_df$`Total Points`[rw] > HEI_calc_df$`Max Points`[rw]) HEI_calc_df$`Cutoff points`[rw] <- HEI_calc_df$`Max Points`[rw]
      else HEI_calc_df$`Cutoff points`[rw] <- HEI_calc_df$`Total Points`[rw]
    }
    if (HEI_cat == "HEI_total_wholeGrains_oz"){
      threshold <- 3 # Should be ≥ 3 oz
      HEI_calc_df$`Total Points`[rw] <- HEI_calc_df$`Daily Avg.`[rw]/threshold * HEI_calc_df$`Max Points`[rw]
      if (HEI_calc_df$`Total Points`[rw] > HEI_calc_df$`Max Points`[rw]) HEI_calc_df$`Cutoff points`[rw] <- HEI_calc_df$`Max Points`[rw]
      else HEI_calc_df$`Cutoff points`[rw] <- HEI_calc_df$`Total Points`[rw]
    }
  }
  
  # Saturated fat goal : for 10 points ≤8% of energy and 0 points for ≥16% of energy
  saturated_fat_total <- sum(intrv_diet$SFA)
  sat_fat_daily <- saturated_fat_total/7
  sat_fat_percent <- sat_fat_daily/daily_total_energy * 100
  
  sat_fat_point <- ifelse(sat_fat_percent >= 16,0,NA)
  
  sat_fat_point <- ifelse(sat_fat_percent <= 8, 10, sat_fat_point)
  
  sat_fat_tot_point <- 10 - (10*(sat_fat_percent-8)/(16-8))
  
  sat_fat_point <- ifelse(is.na(sat_fat_point), sat_fat_tot_point, sat_fat_point)
  
  sat_fat_save_rw <- which(HEI_calc_df$HEI_category == "Saturated Fats")
  HEI_calc_df[sat_fat_save_rw, "Totals"] <- saturated_fat_total
  HEI_calc_df[sat_fat_save_rw, "Daily Avg."] <- sat_fat_daily
  HEI_calc_df[sat_fat_save_rw, "Cutoff points"] <- sat_fat_point
  HEI_calc_df[sat_fat_save_rw, "Total Points"] <- sat_fat_tot_point
  
  # Fatty acid goal ≥2.5  ≤1.5 or 1.2?
  # (Total Monounsaturated Fatty Acids + Total Polyunsaturated Fatty Acids)/Total Saturated Fatty Acids
  total_MUFA <- sum(intrv_diet$MUFA)
  total_PUFA <- sum(intrv_diet$PUFA)
  fatty_acid_ratio <- (total_MUFA + total_PUFA)/saturated_fat_total
  print(paste("fatty_acid:",fatty_acid_ratio))
  
  fa_points <- ifelse(saturated_fat_total == 0 & total_MUFA == 0,10, NA)
  fa_points <- ifelse(fatty_acid_ratio >= 2.5, 10, NA)
  fa_points <- ifelse(saturated_fat_total == 0 & total_MUFA > 0, 10, fa_points)
  fa_points <- ifelse(fatty_acid_ratio >= 2.5, 10,fa_points)
  fa_points <- ifelse(fatty_acid_ratio <= 1.2,0, fa_points)
  fa_tot_point <- 10*((fatty_acid_ratio-1.2)/(2.5-1.2))
  fa_points <- ifelse(is.na(fa_points), fa_tot_point, fa_points)
  fa_save_rw <- which(HEI_calc_df$HEI_category == "Fatty Acids")
  HEI_calc_df[fa_save_rw, "Totals"] <- fatty_acid_ratio
  HEI_calc_df[fa_save_rw, "Daily Avg."] <- fatty_acid_ratio
  HEI_calc_df[fa_save_rw, "Cutoff points"] <- fa_points
  HEI_calc_df[fa_save_rw, "Total Points"] <- fa_tot_point
  
  
  # Sodium allows a total of 10 points and should be between 1.1 and 2.0 grams per 1000 kcal
  # or ≤2.2g	and ≥4g per 2000 kcal
  # Used as reference: https://github.com/timfolsom/hei/blob/master/R/hei.R
  sodium_total <- sum(intrv_diet$Sodium)
  sodium <- sum(intrv_diet$Sodium)/7/daily_total_energy
  sodium_low_threshold <- 1.1/1000*daily_total_energy
  sodium_high_thres <- 2/1000*daily_total_energy
  sodium_point <- ifelse(sodium <= sodium_low_threshold,10,NA)
  sodium_point <- ifelse(sodium >= sodium_high_thres, 0, sodium_point)
  sodium_tot_point <- 10 - (10*(sodium - sodium_low_threshold)/(sodium_high_thres - sodium_low_threshold))
  sodium_point <- ifelse(is.na(sodium_point), sodium_tot_point, sodium_point)
  
  print(paste("sodium: amount, point; ", sodium, sodium_point))
  sodium_save_rw <- which(HEI_calc_df$HEI_category == "Sodium")
  HEI_calc_df[sodium_save_rw, "Totals"] <- sodium_total
  HEI_calc_df[sodium_save_rw, "Daily Avg."] <- sodium
  HEI_calc_df[sodium_save_rw, "Cutoff points"] <- sodium_point
  HEI_calc_df[sodium_save_rw, "Total Points"] <- sodium_tot_point
  
  ##### Store the results in the big table #####
  HEI_score <- sum(HEI_calc_df$`Cutoff points`)
  HEI_scores <- c(HEI_scores, HEI_score)
  HEI_calc_df$Intervention <- rep(intrvntn, nrow(HEI_calc_df))
  HEI_calc_df$Final_score <- rep(HEI_score, nrow(HEI_calc_df))
  
  ##### Debug details #####
  intrv_sum_gram <- c(intrv_sum_gram, sum(intrv_diet$Gram_weight))
  intrv_sum_sug_conv <- c(intrv_sum_sug_conv, sum(intrv_diet$HEI_addedSug_tsp_convs))
  intrv_sum_sug_HEI <- c(intrv_sum_sug_HEI, sum(intrv_diet$HEI_addedSug_tsp))
  intrv_count_sug_conv <- c(intrv_count_sug_conv, sum(intrv_diet$HEI_addedSug_tsp_convs > 0))
  
  if (nrow(big_table) == 0){
    big_table <- HEI_calc_df
  }else{
    big_table <- rbind(big_table, HEI_calc_df)
  }
  
}
names(HEI_scores) <- intrvntns
for (i in 1:length(HEI_scores)){
  print(names(HEI_scores)[i])
  print(paste("HEI_scores", HEI_scores[i]))
  print(paste("intrv_sum_gram", intrv_sum_gram[i]))
  print(paste("intrv_sum_sug_conv", intrv_sum_sug_conv[i]))
  print(paste("intrv_count_sug_conv", intrv_count_sug_conv[i]))
  print(paste("intrv_sum_sug_HEI", intrv_sum_sug_HEI[i]))
  print(paste(" "))
}

write.csv(big_table, file = file.path("data", "diet", "nutrition_data", "MAP_MED_HEI_scores.csv"),
          row.names = FALSE)

print("Reached end of script!")

