# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for building dataframes of the same response variable from each of the
# sites random forest feature importance tables outputs. The purpose is to feed 
# each single response variable dataframe to the combined output Sankey script to
# see if they all agree on the same important features.

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
                        default=file.path("output", "norm_meat_single", "tables"), 
                        help="dir with input 'scores' files"),
  optparse::make_option(c("-o", "--out_suffix"), type="character",
                        default = "site_comb.csv",
                        help="Suffix of output csv."),
  optparse::make_option(c("-u", "--out_dir"), type="character",
                        default = file.path("output", "norm_meat_single"),
                        help="Path of output directory."),
  optparse::make_option(c("-d", "--data_pattern"), type="character",
                        default = "demo-log-filt_all_bat_norm_imput-chem-meats_g.csv",
                        # default="demo-log-filt_all_bat_norm_imput",
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
dir.create(output_dir)
dir.create(file.path(output_dir, "graphics"))
dir.create(file.path(output_dir, "tables"))

clean_sites <- c("PSU_MED", "MB_IIT", "Purdue", "USDA_MED")
dir_files <- list.files(file.path(opt$in_dir), pattern = ".csv")

#### Scrape dir to find data ####
# Take only our "group" by filtering for group pattern
if (opt$data_pattern != FALSE){
  dir_files <- Filter(function(x) grepl(opt$data_pattern, x), dir_files)
}
# take only files from our metabolomics level
if (opt$metblmcs_lev != FALSE){
  dir_files <- Filter(function(x) grepl(paste0("-",opt$metblmcs_lev,"-"), x), dir_files)
}
# use grep with multiple options to filter dir_files
if (opt$accuracy_pattern != FALSE){
  dir_files <- Filter(function(x) grepl(opt$accuracy_pattern, x), dir_files)
}
# Filter for only listed SITEs
# hack for match against multiple arguments
dir_files <- Filter(function(x) grepl(paste(clean_sites,collapse="|"), x), dir_files)
print(paste("data files found:", paste(dir_files, collapse = ", ")))

#### Reorganize dataframes to combine terms ####
my_resp_vars <- c()
all_sites <- c()
for (fl in dir_files) {
  my_file <- read.csv(file.path(opt$in_dir, fl), check.names = FALSE)
  my_site <- clean_sites[unlist(lapply(clean_sites, function(x){grepl(x, fl)}))]#determines
  all_sites <- c(all_sites, my_site)
  my_resp_vars <- c(my_resp_vars, my_file$response_var)
}

print(table(my_resp_vars))

for (rv in unique(my_resp_vars)) {
  my_tabl_name <- paste("reorg", rv, opt$accuracy_pattern, opt$data_pattern,
                        ifelse(opt$metblmcs_lev != FALSE, opt$metblmcs_lev, ""),
                        ".csv", sep = "-")
  my_df <- data.frame()
  my_sites <- c()
  for (fl in dir_files){
    my_file <- read.csv(file.path(opt$in_dir, fl), check.names = FALSE)
    my_site <- clean_sites[unlist(lapply(clean_sites, function(x){grepl(x, fl)}))]#determines
    print(my_site)
    if (length(my_site) > 0){
      print("mysite long")
      my_rows <- my_file[my_file$response_var == rv,]
      my_sites <- c(my_sites, rep(my_site, nrow(my_rows)))
      # print(my_rows)
      if(nrow(my_df) == 0){
        my_df <- my_rows
      }else{
        # stopifnot(identical(names(my_file), my_df))
        my_df <- rbind(my_df, my_rows)
      }
    }else{print("my_site short")}
  }# for loop for files
  my_df$response_var <- unlist(my_sites)
  names(my_df)[names(my_df) == "response_var"] <- "SITE"
  write.csv(my_df, file = file.path(output_dir, "tables", my_tabl_name),
            row.names = FALSE)
}# For loop for response vars

print("End R Script!")


