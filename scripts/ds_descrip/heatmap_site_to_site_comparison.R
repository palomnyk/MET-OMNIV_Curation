# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for comparing scores from from the site to site random forest

rm(list = ls()) #clear workspace

print(paste("Working in", getwd()))

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!require("ggplot2")) BiocManager::install("ggplot2")
library("ggplot2")
if (!requireNamespace("optparse", quietly = TRUE)) BiocManager::install("optparse")
library("optparse")

print("Loaded packages")

#### Parse commandline arguements ####
option_list <- list(
  optparse::make_option(c("-m", "--metblmcs_lev"), type="character", 
                        default=FALSE,
                        help="pathway_level, keyword to help find right random forest scores"),
  optparse::make_option(c("-i", "--in_dir"), type="character", 
                        default=file.path("output", "ml_eval", "tables"), 
                        help="dir with input 'scores' files"),
  optparse::make_option(c("-o", "--out_subdir"), type="character", 
                        default=file.path("output/ml_eval"), 
                        help="dataset dir path"),
  optparse::make_option(c("-p", "--outname"), type="character", 
                        default="output/ml_eval/graphics/heatmap.pdf", 
                        help="prefix in output file name"),
  optparse::make_option(c("-g", "--group_pattern"), type="character", 
                        default="-demo-log-filt_all_bat_norm_imput-chem-meats_g_per_kg_bw-train_test_sep",
                        # default="demo-log-filt_all_bat_norm_imput-chem-meats_g-train_test_sep",
                        # default="demo-log-filt_all_bat_norm_imput-chem-meats_g_per_bmi-train_test_sep",
                        help="pattern to separate score files if there are more than one group in the dir"),
  
  optparse::make_option(c("-c", "--cancel_pattern"), type="character", 
                        default="", 
                        help="pattern to negatively select for - useful if there are more than one group with a similar pattern")
);
opt_parser <- optparse::OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

print("Commandline arguments:")
print(opt)

#### Establish directory layout and other constants ####
base_dir <- file.path("data", "mapping")
correct_sites <- c("PSU-MED","MB/IIT","Purdue","USDA-MAP","USDA-MED")
clean_sites_single <- c("PSU_MED", "MB_IIT", "Purdue", "USDA_MED")
clean_sites <- c("PSU_MED", "MB_IIT", "Purdue", "USDA_MED", "all_sites", "PSU_MED_MB_IIT","PSU_MED_Purdue","PSU_MED_USDA_MED","MB_IIT_Purdue","MB_IIT_USDA_MED",
                "Purdue_USDA_MED","PSU_MED_MB_IIT_Purdue","PSU_MED_MB_IIT_USDA_MED","PSU_MED_Purdue_USDA_MED",
                "MB_IIT_Purdue_USDA_MED")
group_pattern <- gsub("\\'", "", opt$group_pattern)
print(paste("clean group pattern:", group_pattern))
# location for saving big table
bt_file_path <-file.path(opt$out_subdir,"tables",
                         paste0("heatmap_data", gsub("_scores", "", group_pattern), ".csv"))

#### Scrape dir to find data ####
dir_files <- list.files(file.path(opt$in_dir), pattern = "_scores.csv")
# Take only our "group" by filtering for group pattern
if (group_pattern != FALSE){
  dir_files <- Filter(function(x) grepl(group_pattern, x), dir_files)
}
# take only files from our metabolomics level
if (opt$metblmcs_lev != FALSE){
  dir_files <- Filter(function(x) grepl(paste0(opt$metblmcs_lev,"-"), x), dir_files)
}
# use grep with multiple options to filter dir_files
# dir_files <- Filter(function(x) grepl(paste(clean_sites, collapse = "|"), x),
#                     dir_files)

print(paste("data files found:", paste(dir_files, collapse = ", ")))

# create empty vectors to fill
response_var <- vector(mode = "character")
normalization <- vector(mode = "character")
training <- vector(mode = "character")
testing <- vector(mode = "character")
score <- vector(mode = "numeric")
score_path <- vector(mode = "character")
shap_path <- vector(mode = "character")

##### Iterate through files and populate variables #####
for (i in 1:length(dir_files)){
  dat_f <- dir_files[i]
  my_split <- unlist(strsplit(dat_f, "-test_"))
  my_pred <- gsub("train_", "", my_split[1])
  my_resp <- unlist(strsplit(my_split[2], paste0(group_pattern)))[1]
  print(paste("Pred:", my_resp, "Resp:", my_resp))
  meat_split <- unlist(strsplit(dat_f, "-meats_"))[2]
  meat_split <- gsub("-train_test_sep_scores.csv", "", meat_split)
  dat_f_path <- file.path(opt$in_dir, dat_f)
  shap_fname <- paste0("shap_feat_imp_", gsub("_scores", "", dat_f))
  if (file.size(dat_f_path) > 0){
    my_table <- read.csv(dat_f_path, header = T)
    for (r in 1:nrow(my_table)){
      # if(my_table$response_var[r] %in% c("beef", "chicken", "pork", "turkey", "processed", "meat")){
      response_var <- c(response_var, rep(my_table$response_var[r], length(3:ncol(my_table))))
      normalization <- c(normalization, rep(meat_split, length(3:ncol(my_table))))
      training <- c(training, rep(my_pred, length(3:ncol(my_table))))
      testing <- c(testing, rep(my_resp, length(3:ncol(my_table))))
      score <- c(score, unlist(my_table[r, 3:ncol(my_table)]))
      score_path <- c(score_path, rep(dat_f_path, length(3:ncol(my_table))))
      shap_path <- c(shap_path, rep(file.path(opt$in_dir, shap_fname), length(3:ncol(my_table))))
      # }
    }
  }else{
    print(paste(dat_f, "is empty"))
  }
}

big_table <- data.frame(response_var, normalization, training, testing, score, score_path, shap_path)
big_table <- big_table[order(big_table$response_var),]

overlap <- vector(length = nrow(big_table))
for (rw in 1:nrow(big_table)){
  same_cs <- FALSE
  for (cs in clean_sites_single)
    if (grepl( cs, big_table$training[rw]) & grepl(cs, big_table$testing[rw])){
      same_cs <- TRUE
    }
  if (big_table$training[rw] == "all_sites" | big_table$testing[rw] == "all_sites"){
    same_cs <- TRUE
  }
  overlap[rw] <- same_cs
}
big_table$site_overlap <- overlap

print(bt_file_path)

write.csv(big_table, file = bt_file_path,
            row.names = FALSE)

# response_var <-response_var[response_var != "PARENT_SAMPLE_NAME"]
pdf(opt$outname)
for (ur in unique(response_var)){
  print(ur)
  my_df <- big_table[big_table$response_var == ur, c("testing", "training", "score", "site_overlap")]
  my_df[my_df <= 0] <- 0
  
  g <- ggplot(my_df, aes(x = testing, y = training, fill = score)) +
    geom_tile() +
    geom_point( aes(x = testing, y = training, size=ifelse(site_overlap, 0.1, NA)) ) +
    scale_fill_gradient(low = "white", high = "red") +
    labs(title = paste(ur, group_pattern), size = "Site_overlap") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  print(g)
}

dev.off()

print("End of R script")

