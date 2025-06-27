# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for subsetting demo + data files (data/metabolomics/demo-log-filt_all_bat_norm_imput-{metblmcs_lev}.csv)
# by demographics (sex, bmi, and age)

rm(list = ls()) #clear workspace
chooseCRANmirror(graphics=FALSE, ind=66)

### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("optparse", quietly = TRUE)) BiocManager::install("optparse")
library("optparse")

print("Loaded dependencies")

#### Functions ####

#### Parse command line arguments ####
option_list <- list(
  optparse::make_option(c("-d", "--demo_met"), type="character",
                        default = "data/metabolomics/demo-log-filt_all_bat_norm_imput-chem.csv",
                        help="path of metabolomic data"),
  optparse::make_option(c("-o", "--outdir"), type="character",
                        default = "data/metabolomics",
                        help="Path of output csv.")
  
);
opt_parser <- optparse::OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

print("Commandline arguments:")
print(opt)

#### Establish directory layout and other constants ####
output_dir <- file.path(dirname(opt$outdir))
dir.create(output_dir)
suffix_fname <- basename(opt$demo_met)

#### Loading in data ####
in_df <- read.csv(opt$demo_met, header = T, check.names = F)

print("Data loaded!")

#### Subset by Sex and save ####
sex_df <- in_df[in_df$sex == 1, ]
outfile <- paste0(opt$outdir, "/all_site-male_only-", suffix_fname)
write.csv(sex_df, file = outfile, row.names = FALSE)
print(outfile)

sex_df <- in_df[in_df$sex == 0, ]
outfile <- paste0(opt$outdir, "/all_site-female_only-", suffix_fname)
write.csv(sex_df, file = outfile,row.names = FALSE)
print(outfile)

#### Subset age and save ####
age_no_na_df <- in_df[!is.na(in_df$age),]
quantiles <- quantile(in_df$age, na.rm = TRUE)

first_qt <- round(quantiles[1],2)

for (q in 2:length(quantiles)){
  sec_qt <- round(quantiles[q],2)
  group_qunt <- age_no_na_df[age_no_na_df$age <= sec_qt & age_no_na_df$age >= first_qt, ]
  outfile <- paste0(opt$outdir, "/all_site-", "age",
                    as.character(first_qt),"_",as.character(sec_qt),"-",
                    suffix_fname)
  print(outfile)
  write.csv(group_qunt, file = outfile, row.names = FALSE)
  first_qt <- sec_qt
}

#### Subset bmi and save ####
bmi_no_na_df <- in_df[!is.na(in_df$bmi),]
quantiles <- quantile(in_df$bmi, na.rm = TRUE)

first_qt <- round(quantiles[1],2)

for (q in 2:length(quantiles)){
  sec_qt <- round(quantiles[q],2)
  group_qunt <- bmi_no_na_df[bmi_no_na_df$bmi <= sec_qt & bmi_no_na_df$bmi >= first_qt, ]
  outfile <- paste0(opt$outdir, "/all_site-", "bmi",
                    as.character(first_qt),"_",as.character(sec_qt),"-",
                    suffix_fname)
  print(outfile)
  write.csv(group_qunt, file = outfile, row.names = FALSE)
  first_qt <- sec_qt
}

print("Reached end of R script!")

