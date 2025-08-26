# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for finding the site/model combination that has the highest score and
# the one that has the highest versitility for each meat category (response_var).

rm(list = ls()) #clear workspace

print(paste("Working in", getwd()))

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!require("ggplot2")) BiocManager::install("ggplot2")
library("ggplot2")
if (!require("ggpubr")) BiocManager::install("ggpubr")
library("ggpubr")
if (!requireNamespace("rstatix", quietly = TRUE)) BiocManager::install("rstatix")
library("rstatix")
if (!requireNamespace("optparse", quietly = TRUE)) BiocManager::install("optparse")
library("optparse")

print("Loaded packages")

#### Parse commandline arguements ####
option_list <- list(
  optparse::make_option(c("-m", "--metblmcs_lev"), type="character", 
                        default="chem",
                        help="pathway_level, keyword to help find right random forest scores"),
  optparse::make_option(c("-i", "--in_dir"), type="character", 
                        default=file.path("output", "norm_meat", "tables"), 
                        help="dir with input 'scores' files"),
  optparse::make_option(c("-s", "--out_dir"), type="character", 
                        default=file.path("output/cros_val_vs_full_ds"), 
                        help="dataset dir path"),
  optparse::make_option(c("-o", "--out_file"), type="character", 
                        help="dataset dir path",
                        default=file.path("output/no_map/graphics/compr_norm.pdf")),
  optparse::make_option(c("-p", "--out_prefix"), type="character", 
                        default=file.path("auto_prot_"), 
                        help="prefix in output file name"),
  optparse::make_option(c("-g", "--group_pattern"), type="character", 
                        default="-demo-log-filt_all_bat_norm_imput-chem-",
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
output_dir <- file.path(opt$out_dir)
dir.create(output_dir)
dir.create(file.path(output_dir, "tables"))
dir.create(file.path(output_dir, "graphics"))

base_dir <- file.path("data", "mapping")
correct_sites <- c("PSU-MED","MB/IIT","Purdue","USDA-MAP","USDA-MED")
clean_sites <- c("PSU_MED", "MB_IIT", "Purdue", "USDA_MED", "all_sites", "PSU_MED_MB_IIT","PSU_MED_Purdue","PSU_MED_USDA_MED","MB_IIT_Purdue","MB_IIT_USDA_MED",
                 "Purdue_USDA_MED","PSU_MED_MB_IIT_Purdue","PSU_MED_MB_IIT_USDA_MED","PSU_MED_Purdue_USDA_MED",
                 "MB_IIT_Purdue_USDA_MED")
normalizations <- c("meats_g", "meats_g_per_kg_bw", "meats_g_per_bmi", "meats_rmOut_g", "meats_rmOut_g_per_kg_bw", "meats_rmOut_g_per_bmi")
long_endings <- c( "_g_per_kg_bw", "_g_per_bmi", "_rmOut_g", "_rmOut_g_per_kg_bw", "_rmOut_g_per_bmi")
short_endings <- c("_g","_rmOut_g")

group_pattern <- gsub('\\"', "", opt$group_pattern)

#### Scrape dir to find data ####
# Combine variables to make full strings of files
expected_files <- do.call(paste0, expand.grid(clean_sites, group_pattern, normalizations, "_scores.csv"))

dir_files <- list.files(file.path(opt$in_dir), pattern = "_scores.csv")
# # Take only our "group" by filtering for group pattern
# if (group_pattern != FALSE){
#   dir_files <- Filter(function(x) grepl(group_pattern, x), dir_files)
# }

dir_files <- Filter(function(x) x %in% expected_files, dir_files)

print(paste("Using group_pattern:", group_pattern))
# print(paste("data files found:", paste(dir_files, collapse = ", ")))
print(paste("data files found:", length(dir_files)))

# create empty vectors to fill
response_var <- vector(mode = "character")
site_name <- vector(mode = "character")
score <- vector(mode = "numeric")
normalization <- vector(mode = "numeric")
score_path <- vector(mode = "character")
shap_path <- vector(mode = "character")
group_name <- vector(mode = "character")

##### Iterate through files and populate variables #####
for (i in 1:length(dir_files)){
  dat_f <- dir_files[i]
  # print(dat_f)
  my_split <- unlist(strsplit(dat_f, group_pattern))
  my_site <- clean_sites[which(sapply(clean_sites, function(x)(x == my_split[1])))]
  remain <- gsub("_scores.csv", "", my_split[2])
  my_normaliz <- normalizations[which(sapply(normalizations, function(x)(grepl(x, remain))))]
  if(length(my_normaliz) > 1) print(my_normaliz)
  my_normaliz <- ifelse(length(my_normaliz) == 1, my_normaliz, my_normaliz[2])
  end_normaliz <- gsub("meats_", "", my_normaliz)
  dat_f_path <- file.path(opt$in_dir, dat_f)
  if (file.size(dat_f_path) > 0){
    my_table <- read.csv(dat_f_path, header = T)
    # print(dim(my_table))
    for (r in 1:nrow(my_table)){
      if(!my_table$response_var[r] %in% c("PARENT_f_path")){
        my_r_val <- gsub(paste0("_",end_normaliz), "", my_table$response_var[r])
        response_var <- c(response_var, rep(my_r_val, length(3:ncol(my_table))))
        site_name <- c(site_name, rep(my_site, length(3:ncol(my_table))))
        ave_score <- mean(unlist(my_table[r, 3:ncol(my_table)]))
        score <- c(score, rep(ave_score, length(3:ncol(my_table))))
        normalization <- c(normalization, rep(my_normaliz, length(3:ncol(my_table))))
        group_name <- c(group_name, rep(my_table$response_var[r], length(3:ncol(my_table))))
        score_path <- c(score_path, rep(dat_f_path, length(3:ncol(my_table))))
        shap_fname <- paste0("shap_feat_imp_", my_site,"-demo-log-filt_all_bat_norm_imput-chem-", my_normaliz,".csv")
        shap_path <- c(shap_path, rep(file.path("output", "norm_meat", "tables", shap_fname), length(3:ncol(my_table))))
      }
    }
  }else{
    print(paste(dat_f, "is empty"))
  }
}

big_table <- data.frame(response_var = as.factor(response_var), site_name,
                        normalization = as.factor(normalization), group_name,
                        score_path, score, shap_path)

big_table <- big_table[big_table$response_var != "PARENT_SAMPLE_NAME", ]

#### Find highest accuracy for each response var ####
#create empty df to populate
max_accuracy_table <- data.frame(matrix(nrow=0,ncol=ncol(big_table)))
names(max_accuracy_table) <- names(big_table)

my_resp_vars <- unique(big_table$response_var)
for(rv in my_resp_vars){
  print(rv)
  mini_table <- big_table[big_table$response_var == rv,]
  max_score_row <- which(mini_table$score == max(mini_table$score))[1]
  print(mini_table[max_score_row,])
  max_accuracy_table <- rbind(max_accuracy_table, as.data.frame(mini_table[max_score_row,]))
}

#### Make some hand modifications ####
#Remove Purdue as beef_min_proc and replace with the site that is #2
# min_proc_bf_df <- big_table[big_table$response_var == "beef_min_proc", ]
# min_proc_bf_df <- min_proc_bf_df[order(min_proc_bf_df$score, decreasing = TRUE),]
# 
# max_accuracy_table <- max_accuracy_table[-c(which(max_accuracy_table$response_var == "beef_min_proc")),]
# max_accuracy_table <- rbind(as.data.frame(min_proc_bf_df[2,]), max_accuracy_table)

write.csv(max_accuracy_table, file = file.path(output_dir, "tables", "max_accuracy_table.csv"),
          row.names = FALSE)

#### Read table for most general model results & create combo ####
gen_mod_files <- paste0("heatmap_data-demo-log-filt_all_bat_norm_imput-chem-", normalizations, "-train_test_sep.csv")
gen_mod_loc <- file.path("output", "ml_eval", "tables")

big_gm_table <- data.frame()
##### Cycle through scripts/ds_descrip/heatmap_site_to_site_comparison.R output #####
for (gm in gen_mod_files){
  dat_f_path <- file.path(gen_mod_loc, gm)
  my_df <- read.csv(dat_f_path, header = T)
  
  if(nrow(big_gm_table) < 1){
    big_gm_table <- my_df
  }else{
    big_gm_table <- rbind(big_gm_table, my_df)
  }
}

big_gm_table <- within(big_gm_table, rm("testing"))

big_gm_table$group_name <- big_gm_table$response_var
# Aggregate scores of sites and normalizations
agg_gm_table <- aggregate(score ~ training + group_name + shap_path + score_path,
                          big_gm_table, mean)

#add column to hold naked response variables
agg_gm_table$response_var <- sapply(agg_gm_table$group_name, function(x){
  part_gsub <- gsub(paste(long_endings, collapse = "|"),"",x)
  gsub(paste(short_endings, collapse = "|"),"",part_gsub)
})

#### Subset for max values of general models ####
max_gm_table <- data.frame(matrix(nrow=0,ncol=ncol(agg_gm_table)))
names(max_gm_table) <- names(agg_gm_table)
my_resp_vars <- unique(agg_gm_table$response_var)
for(rv in my_resp_vars){
  mini_table <- agg_gm_table[agg_gm_table$response_var == rv,]
  max_score_row <- which(mini_table$score == max(mini_table$score))
  print(mini_table[max_score_row,])
  max_gm_table <- rbind(max_gm_table, as.data.frame(mini_table[max_score_row[1],]))
}

max_combine_table <- merge(max_accuracy_table, max_gm_table, by=c("response_var"),
                           suffixes = c("_crss_val", "_full_dataset"))
max_combine_table$combined_df_path <- rep(NA, nrow(max_combine_table))
for (rw in 1:nrow(max_combine_table)){
  resp_var <- as.character(max_combine_table$response_var[rw])

  out_table_name <- paste0(resp_var, "-", "cross_val_vs_full_ds.csv")
  cross_val_group_name <- max_combine_table$group_name_crss_val[rw]
  cross_val_group_label <- paste(cross_val_group_name, 
                                max_combine_table$site_name[rw],
							  collapse="\n")
  full_ds_group_name <- max_combine_table$group_name_full_dataset[rw]
  full_ds_group_label <- paste(full_ds_group_name, 
                              max_combine_table$training[rw],
                              round(max_combine_table$score_full_dataset[rw],3),
							  collapse="\n")
  cros_val_shap_df <- read.csv(file = max_combine_table$shap_path_crss_val[rw],
                              check.names = FALSE)
  cros_val_shap_df <- cros_val_shap_df[cros_val_shap_df$response_var == cross_val_group_name,]
  cros_val_shap_df$response_var <- rep(cross_val_group_label, nrow(cros_val_shap_df))
  full_ds_shap_df <- read.csv(file = max_combine_table$shap_path_full_dataset[rw],
                               check.names = FALSE)
  full_ds_shap_df <- full_ds_shap_df[full_ds_shap_df$response_var == full_ds_group_name,]
  full_ds_shap_df$response_var <- rep(full_ds_group_label, nrow(full_ds_shap_df))
  out_df <- rbind(cros_val_shap_df, full_ds_shap_df)
  outpath <- file.path(output_dir, "tables", out_table_name)
  max_combine_table$combined_df_path[rw] <- outpath
  write.csv(out_df, file = outpath, row.names = FALSE)
}

write.csv(max_combine_table, file.path(output_dir, "tables", "max_score_combine_table.csv"),
          row.names = FALSE)

print("End of R script")

max_acc_beef_min_row <- which(max_accuracy_table$response_var == "beef_min_proc")

test_cv_beef_min_shap <- read.csv(file = max_accuracy_table$shap_path[max_acc_beef_min_row],
                                   check.names = FALSE)
test_cv_beef_min_score <- read.csv(file = max_accuracy_table$score_path[max_acc_beef_min_row],
                                  check.names = FALSE)

test_cv_beef_min_score$mean_scores <- apply(my_table[, 3:ncol(my_table)], MARGIN = 1, FUN = mean)




