# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for organizing metadata and labeling meat eaten from samples

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
nut_dir <- file.path("data", "diet", "nutrition_data")
metabo_f <- file.path(base_dir,
                      "UARS-01-23ML+ DATA TABLES (DATA ADJUSTED BY BASELINE SAMPLES FROM EACH SITE).xlsx")
metabo_f_new <- file.path(base_dir,"NCBA_siteCorrected_mod.xlsx")
meat_thresholds <- c(0.0, 70) #high beef was 2.5 oz or higher, 2.5 *28 == 69.72 round to 70
meat_threshold_names <- c("no", "low", "high")

#### Loading in data ####
meta_df <- readxl::read_excel(metabo_f, sheet = "Sample Meta Data")

# Site CVM data
usda_med_df <- readxl::read_excel(file.path("data","mapping","USDA PSU Med Beef Data.xlsx"))
mb_meta <- read.csv(file.path("data","mapping","mb", "2112_V1   .csv"), 
                    check.names = FALSE)
mb2_meta <- read.csv(file.path("data","diet","mb", "2112 V2 .csv"), 
                     check.names = FALSE)
mb_map <- readxl::read_excel(file.path("data","mapping","mb", "MB-2112_Screen and Rand.xlsx"))
purdue_meta <- readxl::read_excel(file.path("data","mapping", "purdue", "AY_sample_codebook 9.12.2024 Campbell data.xlsx"),
                                   sheet = "Blood, BP< Anthropemetrics")
purdue_meta$short_id <- sapply(strsplit(purdue_meta$`Participant ID`, "-"), "[", 3)#take 3rd part of split

#### Cycle through site data and build new table ####
# Needed cols: 
#MED: "TRIG mg/dL"  "CHOL mg/dL"  "dHDL mg/dL"  "APOA1 mg/dL" "APOB mg/dL"  "LDL mg/dL" "Lp(a) mg/dL"   "GLU mg/dL" 
#PUR:                                           "Apo_A1"      "Apo_B"       "LDL"       "Lipoprotein_A"                   "Avg_SBP" "Avg_DBP" "HDL"
# MB:                                                                                                    


# All PUR:
# "BUN"             "Creatinine"      "GFR_T"           "Calcium"        
# [21] "Bilirubin"       "T_Protein"       "Albumin"         "A_Phosphatase"   "ALT"            
# [26] "AST"             "VLDL_P"          "T_LDL"           "Non_HDL"        
# [31] "Rem_Lipoprotein" "D_LDL_III"       "D_LDL_IV"        "T_HDL_P"         "B_HDL_2b"       
# [36] "TC"              "TG"                         "TC_HDL"                     
# [41] "Non_HDL_C"       "Insulin"         "hs_CRP"          "log_CRP"
# "Homocysteine"    "MetS_Traits"
# "Avg_SAD"         "SAD1"           
# [66] "SAD2"            "SAD3"

# all MB:
#   HbA1c (%)	hS-CRP (mg/L)	IL-6 (pg/mL)	TNF-a (pg/mL)	Glucose (mg/dL)
# "Systolic_1"                      
# [21] "Diastolic_1"                      "Pulse_1"                         
# [23] "Time_Taken2"                      "Systolic_2"                      
# [25] "Diastolic_2"                      "Pulse_2"                         
# [27] "Time_Taken3"                      "Systolic_3"                      
# [29] "Diastolic_3"                      "Pulse_3"                         
# [31] "Systolic_Average"                 "Diastolic_Average"               
# [33] "Pulse_Average"                    "Diet_Record_collected"           
# [35] "Meet_All"                         "Waiver_granted"                  
# [37] "Rand_Number_Assigned"             "Study_Product"                   
# [39] "LastFood_Date"                    "LastFood_Time"                   
# [41] "Morning_Medication"               "Morning_Medications_time"        
# [43] "Catheter_time"                    "tm15_blood_draw"                 
# [45] "Standardized_Breakfast_t0"        "Standard_Breakfast_finished_time"
# [47] "Standard_Breakfast_Finished"      "t30_blood_draw"                  
# [49] "t60_blood_draw"                   "t120_blood_draw"                 
# [51] "t180_blood_draw"                  "Completion_Meds"                 
# [53] "Completion_Meds_Time"             "Completion_Meds_List"            
# [55] "Ad_Lib_Water"                     "After_blood_draw_AE_assessment"  
# [57] "After_blood_draw_AE_report"       "Study_Food_Dispensed"  

#### Save output ####

write.csv(meta_df, file = file.path("data", "mapping", "CVM_metadata.csv"),
          row.names = FALSE)

meta_df <- meta_df[! is.na(meta_df$beef),]

write.csv(meta_df, file = file.path("data", "mapping", "all_sites_CVM_metadata.csv"),
          row.names = FALSE)

# Remove LCMS technical data for testing in random forest
# meta_df <- meta_df[c("PARENT_SAMPLE_NAME","TIMEPOINT","TREATMENT", agg_columns, names(meat_rows))]
meta_df <- meta_df[c("PARENT_SAMPLE_NAME", agg_columns)]
write.csv(meta_df, file = file.path("data", "mapping", "rf_all_sites_CVM_metadata.csv"),
          row.names = FALSE)

print("End R script.")

