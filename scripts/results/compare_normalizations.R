# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for comparing scores from each normalization faceted by 
# response variables (meat types)

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
  optparse::make_option(c("-s", "--out_subdir"), type="character", 
                        default=file.path("output/no_map/graphics"), 
                        help="dataset dir path"),
  optparse::make_option(c("-t", "--out_file_tree"), type="character", 
                        help="dataset dir path",
                        default=file.path("output/no_map/graphics/compr_norm_tree.png")),
  optparse::make_option(c("-z", "--out_file_zoom"), type="character", 
                        help="dataset dir path",
                        default=file.path("output/no_map/graphics/compr_norm_zoom.png")),
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
base_dir <- file.path("data", "mapping")
correct_sites <- c("PSU-MED","MB/IIT","Purdue","USDA-MAP","USDA-MED")
clean_sites <- c("PSU_MED", "MB_IIT", "Purdue", "USDA_MED", "all_sites", "PSU_MED_MB_IIT","PSU_MED_Purdue","PSU_MED_USDA_MED","MB_IIT_Purdue","MB_IIT_USDA_MED",
                 "Purdue_USDA_MED","PSU_MED_MB_IIT_Purdue","PSU_MED_MB_IIT_USDA_MED","PSU_MED_Purdue_USDA_MED",
                 "MB_IIT_Purdue_USDA_MED")
normalizations <- c("meats_g", "meats_g_per_kg_bw", "meats_g_per_bmi", "meats_rmOut_g", "meats_rmOut_g_per_kg_bw","meats_rmOut_g_per_BMI")

group_pattern <- gsub('\\"', "", opt$group_pattern)

#### Scrape dir to find data ####
# Combine variables to make full strings of files90/
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
sample_name <- vector(mode = "character")

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
      if(!my_table$response_var[r] %in% c("PARENT_SAMPLE_NAME")){
        my_r_val <- gsub(paste0("_",end_normaliz), "", my_table$response_var[r])
        response_var <- c(response_var, rep(my_r_val, length(3:ncol(my_table))))
        site_name <- c(site_name, rep(my_site, length(3:ncol(my_table))))
        score <- c(score, unlist(my_table[r, 3:ncol(my_table)]))
        normalization <- c(normalization, rep(my_normaliz, length(3:ncol(my_table))))
      }
    }
  }else{
    print(paste(dat_f, "is empty"))
  }
  
}

normalization <- gsub("meats_", "", normalization)
big_table <- data.frame(response_var = as.factor(response_var), site_name,
                        normalization = as.factor(normalization), score)


png(file.path(opt$out_file_tree),
    width = 30, height = 9, units = "in", res = 300)
#### Plot with pvalue tree and outlier ####
g <- ggplot2::ggplot(big_table, aes(x=normalization, y=score)) +
  geom_boxplot() +
  ggplot2::ylab("Score") +
  # ggplot2::ggtitle(label = paste(my_title)) +
  # geom_text(data = means, aes(label=paste("m:",round_m), y=m + 0.1, x = 3), color="black") +
  ggplot2::scale_x_discrete(guide = guide_axis(angle = 90)) +
  # ggplot2::stat_summary(fun=mean, geom="point", shape="-", size=9, color="red", fill="red") +
  # coord_cartesian(ylim = c(-0.5,NA)) +
  ggplot2::facet_grid(~ response_var) +
  ggpubr::stat_compare_means(paired=FALSE, method = "kruskal.test",
                             hide.ns = TRUE) +
  ggpubr::geom_pwc(method = "wilcox.test", hide.ns = TRUE)
g <- ggpubr::ggadjust_pvalue(
  g, p.adjust.method = "BH",
  label = "{p.adj.format}{p.adj.signif}", hide.ns = TRUE)
# Note that, tests such as tukey_hsd or games_howell_test handle p-value adjustement internally; they only return the p.adj. 

print(g)
dev.off()

##### Plot with clear boxplots
png(file.path(opt$out_file_zoom),
    width = 30, height = 9, units = "in", res = 300)
g <- ggplot2::ggplot(big_table, aes(x=normalization, y=score)) +
  geom_boxplot() +
  ggplot2::ylab("Score") +
  # ggplot2::ggtitle(label = paste(my_title)) +
  # geom_text(data = means, aes(label=paste("m:",round_m), y=m + 0.1, x = 3), color="black") +
  ggplot2::scale_x_discrete(guide = guide_axis(angle = 90)) +
  # ggplot2::stat_summary(fun=mean, geom="point", shape="-", size=9, color="red", fill="red") +
  coord_cartesian(ylim = c(-2,1.5)) +
  ggplot2::facet_grid(~ response_var) +
  ggpubr::stat_compare_means(paired=FALSE, method = "kruskal.test",
                             hide.ns = TRUE)
print(g)

dev.off()

# # https://stackoverflow.com/questions/45047914/how-do-i-annotate-p-values-onto-a-faceted-bar-plots-on-r
# 
# #### Statistics ####
# if (!requireNamespace("rstatix", quietly = TRUE)) BiocManager::install("rstatix")
# library("rstatix")
# # big_table <- tibble(big_table)
# stat_df <- big_table %>%
#   group_by(response_var) %>%
#   t_test(score ~ normalization) %>%
#   adjust_pvalue(method = "BH") %>%
#   add_significance()
# 
# stat_df <- stat_df %>% add_xy_position(x = "supp")
# #### Make plot ####
# big_table$response_var <- as.factor(big_table$response_var)
# # https://www.datanovia.com/en/blog/how-to-add-p-values-to-ggplot-facets/
# g <- ggboxplot(
#   data = big_table, x="normalization", y="score",
#   facet.by = "response_var")
#   # stat_pvalue_manual(
#   #   stat_df, bracket.nudge.y = -2, hide.ns = TRUE,
#   #   label = "{p.adj}{p.adj.signif}")
# # print(g)
# 
# # Next try below:
# # pdf(opt$out_file, width = 18, height = 8)
# 
# my_title <- paste("Group:", group_pattern, "| metabo lev:", opt$metblmcs_lev)
# 
# means <- aggregate(big_table$score, by=list(big_table$response_var), mean)
# names(means) <- c("response_var", "m")
# stdv <- aggregate(big_table$score, by=list(big_table$response_var), sd)
# names(stdv) <- c("response_var", "s")
# means <- merge(means, stdv)
# means$round_m <- round(means$m, 3)
# print(means)
# g <- ggplot2::ggplot(big_table, aes(x=normalization, y=score)) + 
#   geom_boxplot() +
#   ggplot2::ylab("Score") +
#   ggplot2::ggtitle(label = paste(my_title)) +
#   # geom_text(data = means, aes(label=paste("m:",round_m), y=m + 0.1, x = 3), color="black") +
#   ggplot2::scale_x_discrete(guide = guide_axis(angle = 90)) +
#   # ggplot2::stat_summary(fun=mean, geom="point", shape="-", size=9, color="red", fill="red") +
#   coord_cartesian(ylim = c(-0.1,NA)) +
#   
#   ggplot2::facet_grid(~ response_var)
# 
# # https://stackoverflow.com/questions/45047914/how-do-i-annotate-p-values-onto-a-faceted-bar-plots-on-r
# # print(g)
# 
# # dev.off()


print("End of R script")

