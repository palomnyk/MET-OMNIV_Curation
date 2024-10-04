# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for making PCA

rm(list = ls()) #clear workspace

print(paste("Working in", getwd()))

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("openxlsx", quietly = TRUE))  BiocManager::install("openxlsx")
library("openxlsx")
if (!require("ggplot2")) BiocManager::install("ggplot2")
library("ggplot2")

print("Loaded packages")

#### Load functions ####
filter_metabolites <- function(df) {
  print(paste(colnames(df)[1:5], collapse = ", "))
  #Remove columns that all but 2 NA
  df <- df[,!sapply(df, function(x) length(which(is.na(x))) >= nrow(df)-2)]
  
  #Remove features with more than 80% zeros
  na_table <- df # Lauren wants to keep NA as NA for cv filter
  na_table[is.na(na_table)] <- 0
  df <- df[,!sapply(na_table, function(x) mean(x == 0) > 0.8)]
  print(paste("0 rw/col:", paste(dim(df), collapse = "/")))
  # #Exclude metabolites that have a CV>30.0 OR keep metabolites that are < 30
  df <- df[,!sapply(df,function(x) sd(x, na.rm = T) / mean(x, na.rm = T) < 0.3)]
  
  print(paste("CV rw/col:", paste(dim(df), collapse = "/")))
  
  return(names(df))
}

#### Establish directory layout and other constants ####
metabo_f <- file.path("data", "metabolomics", "UARS-01-23ML+",
                      "UARS-01-23ML+ DATA TABLES (ALL SAMPLES).xlsx")
mtbmcs_file <- openxlsx::loadWorkbook(metabo_f)
mtbmcs_sheets <- names(mtbmcs_file)
metad_intrest <- c("controls", "TREATMENT")

#### Loading in data ####
mtbmcs_peak <-openxlsx::read.xlsx(metabo_f,
                                  sheet = "Peak Area Data",
                                  rowNames = TRUE)
mtbmcs_bn <-openxlsx::read.xlsx(metabo_f,
                                sheet = "Batch-normalized Data",
                                rowNames = TRUE)
mtbmcs_bnI <-openxlsx::read.xlsx(metabo_f,
                                 sheet = "Batch-norm Imputed Data",
                                 rowNames = TRUE)
meta_df <- openxlsx::read.xlsx(metabo_f,
                               sheet = "Sample Meta Data",
                               rowNames = TRUE)
chem_link <- openxlsx::read.xlsx(xlsxFile = metabo_f,
                                 sheet = "Chemical Annotation")

names(mtbmcs_bnI) <- chem_link$CHEMICAL_NAME[match(names(mtbmcs_bnI), chem_link$CHEM_ID)]
names(mtbmcs_bn) <- chem_link$CHEMICAL_NAME[match(names(mtbmcs_bn), chem_link$CHEM_ID)]

metabo_f <- file.path("data", "metabolomics", "UARS-01-23ML+",
                      "UARS-01-23ML+ DATA TABLES (DATA ADJUSTED BY BASELINE SAMPLES FROM EACH SITE).xlsx")

#### Filter nmetabolites ####
my_cols <- filter_metabolites(mtbmcs_bn)

filtered_mtbcs <- mtbmcs_bnI[,my_cols]

write.csv(filtered_mtbcs, file.path("data", "metabolomics", "filt_all_bat_norm_imput.csv"),
          row.names = T)

#log2 table
filtered_mtbcs <- log2(filtered_mtbcs)

filtered_mtbcs$PARENT_SAMPLE_NAME <- row.names(filtered_mtbcs)

write.csv(filtered_mtbcs, file.path("data", "metabolomics", "log_filt_all_bat_norm_imput.csv"),
          row.names = FALSE)

print("End R script.")
