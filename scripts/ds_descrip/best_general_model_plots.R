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
  optparse::make_option(c("-i", "--infile"), type="character", 
                        default=file.path("output", "ml_eval", "tables", "heatmap_data-demo-log-filt_all_bat_norm_imput-chem-meats_g_per_bmi-train_test_sep.csv"), 
                        help="input file, should be .csv"),
  optparse::make_option(c("-p", "--outfile"), type="character", 
                        default="output/ml_eval/graphics/best_gen_model.pdf", 
                        help="prefix in output file name")
);
opt_parser <- optparse::OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

print("Commandline arguments:")
print(opt)

#### Establish directory layout and other constants ####
base_dir <- file.path("data", "mapping")
correct_sites <- c("PSU-MED","MB/IIT","Purdue","USDA-MAP","USDA-MED")
clean_sites_single <- c("PSU_MED", "MB_IIT", "Purdue", "USDA_MED")
clean_sites <- c("PSU_MED", "MB_IIT", "Purdue", "USDA_MED", "noMap", "PSU_MED_MB_IIT","PSU_MED_Purdue","PSU_MED_USDA_MED","MB_IIT_Purdue","MB_IIT_USDA_MED",
                 "Purdue_USDA_MED","PSU_MED_MB_IIT_Purdue","PSU_MED_MB_IIT_USDA_MED","PSU_MED_Purdue_USDA_MED",
                 "MB_IIT_Purdue_USDA_MED")
infile <- read.csv(opt$infile)


#### Organize data ####
infile <- infile[infile$site_overlap == FALSE,]

#### Create plot ####


pdf(file = opt$outfile)

for (nm in unique(infile$response_var)){
  sub_table <- infile[infile$response_var == nm, ]
  st_agg <- aggregate(sub_table$score, by=list(sub_table$training), sum)
  names(st_agg) <- c("Training site", "All scores")
  g <- ggplot(sub_table, aes(x = reorder(training, score, fun = median, .desc =TRUE), y = score)) +
    geom_boxplot() +
    labs(title = paste("Sitewise comparison training", nm)) +
    coord_cartesian(ylim = c(-0.5, 1)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  print(g)
  g <- ggplot(sub_table, aes(x = reorder(testing, score, fun = median, .desc =TRUE), y = score)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(-0.5, 1)) +
    labs(title = paste("Sitewise comparison testing", nm)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  print(g)
}

dev.off()

print("End of R script")

