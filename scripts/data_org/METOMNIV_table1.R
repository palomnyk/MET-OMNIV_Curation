# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for making table for protocol paper

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
output_dir <- file.path("output", "protocol")
dir.create(file.path(output_dir))
dir.create(file.path(output_dir, "graphics"))
dir.create(file.path(output_dir, "tables"))
base_dir <- file.path("data", "metabolomics", "UARS-01-23ML+")
nut_dir <- file.path("data", "diet", "nutrition_data")
metabo_f <- file.path(base_dir,
                      "UARS-01-23ML+ DATA TABLES (ALL SAMPLES).xlsx")

#### Load data ####
meta_df <- openxlsx::read.xlsx(metabo_f,
                               sheet = "Sample Meta Data")
demo_data <- read.csv("data/mapping/all_plasma-all_sites_demo.csv", check.names = FALSE,
                      row.names = "PARENT_SAMPLE_NAME")
control_treatments <- c("Med 100","Purple", "Orange","Med 200", "C")
correct_sites <- c("PSU-MED","MB/IIT","Purdue","USDA-MAP","USDA-MED")

table_columns <- c("SITE", "CLIENT_MATRIX", "age", "sex", "bmi")
new_table_columns <- c("SITE", "MATRIX", "Age", "Sex", "BMI")

#### Fix some inconsistencies ####
meta_df$controls <- meta_df$TREATMENT %in% control_treatments
meta_df$TREATMENT[meta_df$TREATMENT == "MED 0.5"] <- "Med 0.5"
meta_df$TREATMENT[meta_df$TREATMENT == "MED 2.5"] <- "Med 2.5"
meta_df$TREATMENT[meta_df$TREATMENT == "MED 5.5"] <- "Med 5.5"
meta_df$TREATMENT[meta_df$TIMEPOINT == "post "] <- "post"
meta_df$TREATMENT[meta_df$TREATMENT == "A"] <- "Chicken"
meta_df$TREATMENT[meta_df$TREATMENT == "B"] <- "Beef"
meta_df$TREATMENT[meta_df$TREATMENT == "BL"] <- "baseline"

print(paste("num non-control:", sum(!meta_df$controls)))
##### Improve readability #####
#Drop incorrect "SITE" column and rename "CORRECTED_SITE" to "SITE"
drops <- c("SITE")
meta_df <- meta_df[ , !(names(meta_df) %in% drops)]
colnames(meta_df)[colnames(meta_df) == "CORRECTED_SITE"] = "SITE"

# Add USUAL diet to MB treatment
meta_df$TREATMENT[meta_df$SITE == "MB/IIT" & meta_df$TIMEPOINT == "baseline"] <- "Usual"
row.names(meta_df) <- meta_df$PARENT_SAMPLE_NAME

#### Add demographic data ####
meta_df <- merge(meta_df, demo_data, all = FALSE, by = 0)
meta_df$sex[meta_df$sex == 1] <- "# male"
meta_df$sex[meta_df$sex == 0] <- "# female"

#### Drop unneeded rows and columns ####
#Convert character columns to strings 
meta_df <- type.convert(meta_df, as.is = FALSE)
#Take only baseline
meta_df <- meta_df[meta_df$TIMEPOINT == "baseline",]
#Take only needed columns and then rename them for publication
meta_df <- meta_df[,table_columns]
names(meta_df) <- new_table_columns

#### Summarize numeric data ####
# Separate numeric data
num_col_tf <- sapply(meta_df, is.numeric)
num_col <- names(meta_df)[num_col_tf]
num_df <- meta_df[,num_col]
num_df$SITE <- meta_df$SITE
num_df$MATRIX <- meta_df$MATRIX

###### Side quest: full dataset age and BMI ######
full_means <-  sapply(num_df[, num_col], mean, na.rm = TRUE)
full_sd <-  sapply(num_df[, num_col], sd, na.rm = TRUE)
print("full_means:")
print(full_means)
print("full_sds")
print(full_sd)

##### Calculate mean and stdv and then round #####
means <- aggregate(. ~ SITE + MATRIX, FUN = mean, data = num_df)
stdv <- aggregate(. ~ SITE + MATRIX, FUN = sd, data = num_df)
save_df <- merge(means, stdv, by=c("SITE", "MATRIX"),
                           suffixes = c("|mean", "|sd"))
save_df[,c(-1,-2)] <-round(save_df[,c(-2, -1)],2) #the "-1" excludes column 1

###### Side quest: calculate combined MED age and BMI for paper ######
full_med <- num_df[num_df$SITE %in% c("PSU-MED", "USDA-MED"),]
med_means <- sapply(num_df[, num_col], mean, na.rm = TRUE)
med_sds <- sapply(num_df[, num_col], sd, na.rm = TRUE)
min_data <- sapply(num_df[, num_col], min, na.rm = TRUE)
max_data <- sapply(num_df[, num_col], max, na.rm = TRUE)

print("MED means:")
print(med_means)
print("MED_sds")
print(med_sds)

##### Reformat mean and sd cols #####
num_df <- data.frame(matrix(ncol=0, nrow=nrow(save_df))) #Empty df for refilling
num_df$SITE <- save_df$SITE
num_df$MATRIX <- save_df$MATRIX
for (nc in num_col){
  ###### Character vector processing ######
  mean_nam <- paste0(nc, "|mean")
  sd_nam <- paste0(nc, "|sd")
  new_name <- paste(nc, "mean +- standard deviation")
  
  ###### Make new column ######
  mean_col <- save_df[,mean_nam]
  sd_col <- save_df[,sd_nam]
  new_col <- paste0(mean_col, " +- ", sd_col)
  num_df[,new_name] <- new_col
}


#### Create factor portion of save data ####
fact_df <- meta_df[,c("SITE", "Sex")]
sex_df <- as.data.frame.matrix(table(fact_df))

save_df <- cbind(num_df, sex_df)

write.csv(save_df, file = file.path(output_dir, "tables", "table1.csv"),
          row.names = FALSE)

save_df$total <- save_df$`# female` + save_df$`# ma`

print("End R script.")

