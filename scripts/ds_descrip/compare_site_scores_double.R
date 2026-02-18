# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for comparing scores of two groups from each site, 
# -used for comparing two groups of demographic data

rm(list = ls()) #clear workspace

print(paste("Working in", getwd()))

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!require("ggplot2")) BiocManager::install("ggplot2")
library("ggplot2")
if (!requireNamespace("optparse", quietly = TRUE)) BiocManager::install("optparse")
library("optparse")
if (!require("ggpubr")) BiocManager::install("ggpubr")
library("ggpubr")

print("Loaded packages")

#### Parse commandline arguements ####
option_list <- list(
  optparse::make_option(c("-m", "--metblmcs_lev"), type="character", 
                        default="chem",
                        help="pathway_level, keyword to help find right random forest scores"),
  optparse::make_option(c("-i", "--in_dir"), type="character", 
                        default=file.path("output", "demo_scor", "tables"), 
                        help="dir with input 'scores' files"),
  optparse::make_option(c("-s", "--out_subdir"), type="character",
                        # default=file.path("output/no_map/graphics"), 
                        help="dataset dir path"),
  optparse::make_option(c("-o", "--out_file"), type="character", 
                        help="output dir path for plots"),
  # optparse::make_option(c("-t", "--out_table"), type="character", 
  #                       help="output dir path for table"),
  optparse::make_option(c("-p", "--out_prefix"), type="character", 
                        default=file.path("auto_prot_"), 
                        help="prefix in output file name"),
  optparse::make_option(c("-n", "--group_name"), type="character", 
                        default="24-42,42-73",
                        help="Name to label the group that the group_pattern finds, can be a comma seperated list as a string"),
  optparse::make_option(c("-g", "--group_pattern"), type="character", 
                        default="-age24_42-demo-log-filt_all_bat_norm_imput-chem-meats_rmOut_g_per_kg_bw_scores.csv,-age42_73-demo-log-filt_all_bat_norm_imput-chem-meats_rmOut_g_per_kg_bw_scores.csv",
                        help="pattern to separate score files if there are more 
                        than one group in the dir, can be a comma seperated list as a string"),
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

##### Preprocess command line options #####
group_pattern <- gsub(('/"'), "", opt$group_pattern, fixed = TRUE)
group_pattern <- gsub(("\\"), "", group_pattern, fixed = TRUE)
group_pattern <- gsub("[[:space:]]", "", group_pattern)
group_pattern <- paste0(group_pattern, "")#hack to remove special char from string literal
group_pats <- unlist(strsplit(group_pattern, split = ","))
group_names <- unlist(strsplit(opt$group_name, split = ","))
normalization <- tail(unlist(strsplit(opt$group_pattern, "-", fixed = TRUE)),n=1)
normalization <- unlist(strsplit(split = "_scores.csv", normalization))[1]
normalization <- gsub("meats_", "", normalization)
print(paste("Normalization:", normalization))

# create empty vectors to fill through iteration
response_var <- vector(mode = "character")
simple_rsp_var <- vector(mode = "character")
site_name <- vector(mode = "character")
score <- vector(mode = "numeric")
gp_pattern <- vector(mode = "character")
gp_name <- vector(mode = "character")

#### Iterate through group patterns ####
for (gp in 1:length(group_pats)){
  my_pat <- group_pats[gp]
  my_name <- group_names[gp]
  ##### Scrape dir to find data for pattern#####
  group_pattern <- gsub('\\"', "", my_pat)
  expected_files <- paste0(clean_sites, group_pattern)
  print(paste("expected file:", expected_files[1]))
  
  dir_files <- list.files(file.path(opt$in_dir), pattern = "_scores.csv")
  # # Take only our "group" by filtering for group pattern
  # if (group_pattern != FALSE){
  #   dir_files <- Filter(function(x) grepl(group_pattern, x), dir_files)
  # }
  
  dir_files <- Filter(function(x) x %in% expected_files, dir_files)
  
  print(paste("Using group_pattern:", group_pattern))
  print(paste("data files found:", paste(dir_files, collapse = ", ")))
  
  ###### Iterate through files and populate variables ######
  for (i in 1:length(dir_files)){
    dat_f <- dir_files[i]
    # print(dat_f)
    my_split <- unlist(strsplit(dat_f, group_pattern))
    my_site <- clean_sites[which(sapply(clean_sites, function(x)(x == my_split[1])))]
    dat_f_path <- file.path(opt$in_dir, dat_f)
    if (file.size(dat_f_path) > 0){
      my_table <- read.csv(dat_f_path, header = T)
      # print(dim(my_table))
      for (r in 1:nrow(my_table)){
        if(!my_table$response_var[r] %in% c("PARENT_SAMPLE_NAME")){
          resp_var <- my_table$response_var[r]
          simp_res_v <- gsub(paste0("_",normalization), "", resp_var, fixed = TRUE)
          response_var <- c(response_var, rep(resp_var, length(3:ncol(my_table))))
          simple_rsp_var <- c(simple_rsp_var, rep(simp_res_v, length(3:ncol(my_table))))
          site_name <- c(site_name, rep(my_site, length(3:ncol(my_table))))
          score <- c(score, unlist(my_table[r, 3:ncol(my_table)]))
          gp_pattern <- c(gp_pattern, rep(my_pat, length(3:ncol(my_table))))
          gp_name <- c(gp_name, rep(my_name, length(3:ncol(my_table))))
        }
      }
    }else{
      print(paste(dat_f, "is empty"))
    }
  }
}

#### Combine tables ####
big_table <- data.frame(simple_rsp_var, response_var, site_name, score, gp_name, gp_pattern)

print(head(big_table))
big_table$label <- paste0(big_table$gp_name, "-", big_table$site)

#### Make table of high scores for each response var ####
hi_score_df <- lapply(unique(big_table$response_var), function(x){
  my_table <- big_table[big_table$response_var == x & big_table$site_name=="all_sites", ]
  hi_scor_row <- which(my_table$score == max(my_table$score))
  return(my_table[hi_scor_row,])
})

hi_score_df <- do.call(rbind.data.frame, hi_score_df)
print(hi_score_df)

hi_score_fname <- tools::file_path_sans_ext(basename(opt$out_file))
write.csv(hi_score_df, file = file.path(dirname(opt$out_subdir), "tables", paste0(hi_score_fname, ".csv")),
          row.names = FALSE)

#### Make plots ####
pdf(opt$out_file, width = 13, height = 5)

for (st in c("all_sites")){
  my_table <- big_table[big_table$site_name == st, ]
  # my_table <- my_table[order(my_table$site_name, decreasing = TRUE),]
  
  title_text <- paste("Site:", st, "| metabo lev:", opt$metblmcs_lev, "| normalization:", normalization)
  
  g <- ggplot2::ggplot(my_table, aes(x=gp_name, y=score)) + 
    geom_boxplot() +
    ggplot2::ylab("Score") +
    ggplot2::ggtitle(label = paste(title_text)) +
    ggplot2::theme(axis.ticks.x = element_blank(),
                   axis.title.x = element_blank()) +
    # geom_text(data = medians, aes(label=paste("m:",round_x), y=1, x = 3), color="black") +
    ggplot2::scale_x_discrete(guide = guide_axis(angle = 90)) +
    # coord_cartesian(ylim = c(-0.1,1)) +
    ggplot2::facet_grid(~simple_rsp_var) +
    # ggpubr::stat_compare_means(paired=FALSE, method = "kruskal.test",
    #                            hide.ns = TRUE) +
    ggpubr::geom_pwc(method = "wilcox.test", hide.ns = TRUE, y.position = 0.90)
  # g <- ggpubr::ggadjust_pvalue(
  #   g, p.adjust.method = "BH",
  #   label = "{p.adj.format}{p.adj.signif}", hide.ns = TRUE)
  # Note that, tests such as tukey_hsd or games_howell_test handle p-value adjustement internally; they only return the p.adj.
  print(g)
  
  
  g <- ggplot2::ggplot(my_table, aes(x=gp_name, y=score)) + 
    geom_boxplot() +
    ggplot2::ylab("Score") +
    ggplot2::ggtitle(label = paste(title_text)) +
    ggplot2::theme(axis.ticks.x = element_blank(),
                   axis.title.x = element_blank()) +
    ggplot2::scale_x_discrete(guide = guide_axis(angle = 90)) +
    coord_cartesian(ylim = c(-0.5,1)) +
    ggplot2::facet_grid(~simple_rsp_var) +
    ggpubr::geom_pwc(method = "wilcox.test", hide.ns = TRUE, y.position = 1)
  print(g)
}

dev.off()

print("End of R script")

