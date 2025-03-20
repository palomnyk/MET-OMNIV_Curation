# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for comparing scores from each site

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
                        default=file.path("-chem-"),
                        help="pathway_level, keyword to help find right random forest scores"),
  optparse::make_option(c("-i", "--in_dir"), type="character", 
                        default=file.path("output", "no_map_auto_protein", "tables"), 
                        help="dir with input 'scores' files"),
  optparse::make_option(c("-o", "--out_subdir"), type="character", 
                        default=file.path("output/no_map_auto_protein/graphics"), 
                        help="dataset dir path")
);
opt_parser <- optparse::OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

print("Commandline arguments:")
print(opt)

#### Functions ####
resp_vars_plot <- function (df_table, title_text){
  means <- aggregate(df_table$score, by=list(df_table$response_var), mean)
  names(means) <- c("response_var", "m")
  stdv <- aggregate(df_table$score, by=list(df_table$response_var), sd)
  names(stdv) <- c("response_var", "s")
  means <- merge(means, stdv)
  means$x <- round(means$m, 3)
  g <- ggplot2::ggplot(df_table, aes(x=site_name, y=score)) + 
    geom_boxplot() +
    ggplot2::ylab("Score") +
    ggplot2::ggtitle(label = paste(title_text)) +
    geom_text(data = means, aes(label=paste("m:",x), y=x + 0.1, x = 3), color="black") +
    ggplot2::scale_x_discrete(guide = guide_axis(angle = 90)) +
    ggplot2::geom_hline(data = means, aes(yintercept = x), color="red", linetype=4) +
    ggplot2::geom_hline(data = means, aes(yintercept = x + s), color="blue", linetype=2) +
    ggplot2::geom_hline(data = means, aes(yintercept = x - s), color="blue", linetype=2) +
    coord_cartesian(ylim = c(-1,1)) +
    ggplot2::facet_grid(~ response_var)
  return(g)
}

#### Establish directory layout and other constants ####
base_dir <- file.path("data", "mapping")
correct_sites <- c("PSU-MED","MB/IIT","Purdue","USDA-MAP","USDA-MED")
clean_sites = c("PSU_MED", "MB_IIT", "Purdue", "USDA_MED", "noMap")

#### Scrape dir to find data ####
dir_files <- list.files(file.path(opt$in_dir), pattern = "_scores.csv")
# take only files from our metabolomics level
dir_files <- Filter(function(x) grepl(opt$metblmcs_lev, x), dir_files)
# use grep with multiple options to filter dir_files
dir_files <- Filter(function(x) grepl(paste(clean_sites, collapse = "|"), x),
                    dir_files)

print(paste("data files found:", paste(dir_files, collapse = ", ")))

# create empty vectors to fill
response_var <- vector(mode = "character")
site_name <- vector(mode = "character")
score <- vector(mode = "numeric")

##### Iterate through files and populate variables #####
for (i in 1:length(dir_files)){
  dat_f <- dir_files[i]
  print(dat_f)
  my_site <- clean_sites[which(sapply(clean_sites, grepl, dat_f))]
  dat_f_path <- file.path(opt$in_dir, dat_f)
  if (file.size(dat_f_path) > 0){
    my_table <- read.csv(dat_f_path, header = T)
    for (r in 1:nrow(my_table)){
      response_var <- c(response_var, rep(my_table$response_var[r], 10))
      site_name <- c(site_name, rep(my_site, 10))
      score <- c(score, unlist(my_table[r, 3:ncol(my_table)]))
    }
  }else{
    print(paste(dat_f, "is empty"))
  }
  
}

big_table <- data.frame(response_var, site_name, score)

resp_vars_plot(big_table, "Chemical Name")





