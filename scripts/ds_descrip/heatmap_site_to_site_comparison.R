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
                        default="demo-log-filt_all_bat_norm_imput-chem-meats_g_per_kg_bw-train_test_sep_train_test_sep",
                        # default="demo-log-filt_all_bat_norm_imput-chem-meats_g-train_test_sep_train_test_sep", 
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
clean_sites <- c("PSU_MED", "MB_IIT", "Purdue", "USDA_MED", "noMap", "PSU_MED_MB_IIT","PSU_MED_Purdue","PSU_MED_USDA_MED","MB_IIT_Purdue","MB_IIT_USDA_MED",
                "Purdue_USDA_MED","PSU_MED_MB_IIT_Purdue","PSU_MED_MB_IIT_USDA_MED","PSU_MED_Purdue_USDA_MED",
                "MB_IIT_Purdue_USDA_MED")

#### Scrape dir to find data ####
dir_files <- list.files(file.path(opt$in_dir), pattern = "_scores.csv")
# Take only our "group" by filtering for group pattern
if (opt$group_pattern != FALSE){
  dir_files <- Filter(function(x) grepl(opt$group_pattern, x), dir_files)
}
# take only files from our metabolomics level
if (opt$metblmcs_lev != FALSE){
  dir_files <- Filter(function(x) grepl(paste0("-",opt$metblmcs_lev,"-"), x), dir_files)
}
# use grep with multiple options to filter dir_files
# dir_files <- Filter(function(x) grepl(paste(clean_sites, collapse = "|"), x),
#                     dir_files)

print(paste("data files found:", paste(dir_files, collapse = ", ")))

# create empty vectors to fill
response_var <- vector(mode = "character")
pred_site <- vector(mode = "character")
resp_site <- vector(mode = "character")
score <- vector(mode = "numeric")

##### Iterate through files and populate variables #####
for (i in 1:length(dir_files)){
  dat_f <- dir_files[i]
  print(dat_f)
  my_split <- unlist(strsplit(dat_f, "-test_"))
  my_pred <- gsub("train_", "", my_split[1])
  my_resp <- unlist(strsplit(my_split[2], paste0("-", opt$group_pattern)))[1]
  print(paste("Pred:", my_resp, "Resp:", my_resp))
  dat_f_path <- file.path(opt$in_dir, dat_f)
  if (file.size(dat_f_path) > 0){
    my_table <- read.csv(dat_f_path, header = T)
    for (r in 1:nrow(my_table)){
      # if(my_table$response_var[r] %in% c("beef", "chicken", "pork", "turkey", "processed", "meat")){
      response_var <- c(response_var, rep(my_table$response_var[r], length(3:ncol(my_table))))
      pred_site <- c(pred_site, rep(my_pred, length(3:ncol(my_table))))
      resp_site <- c(resp_site, rep(my_resp, length(3:ncol(my_table))))
      score <- c(score, unlist(my_table[r, 3:ncol(my_table)]))
      # }
    }
  }else{
    print(paste(dat_f, "is empty"))
  }
}

big_table <- data.frame(response_var, pred_site, resp_site, score)
big_table <- big_table[order(big_table$response_var),]
write.table(big_table, file = file.path(opt$out_subdir,
                                        paste0("heatmap_data_",opt$group_pattern, ".csv")),
            row.names = FALSE)

# response_var <-response_var[response_var != "PARENT_SAMPLE_NAME"]
# pdf(opt$outname)
for (ur in unique(response_var)){
  print(ur)
  my_df <- big_table[big_table$response_var == ur, c("resp_site", "pred_site", "score")]
  my_df[my_df <= 0] <- 0
  
  g <- ggplot(my_df, aes(x = resp_site, y = pred_site, fill = score)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "red") +
    labs(title = paste(ur, opt$group_pattern)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  print(g)
}

# dev.off()

print("End of R script")

