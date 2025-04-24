# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for comparing scores from each site

rm(list = ls()) #clear workspace

print(paste("Working in", getwd()))

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!require("VennDiagram")) BiocManager::install("VennDiagram")
library("VennDiagram")
if (!require("RColorBrewer")) BiocManager::install("RColorBrewer")
library("RColorBrewer")
if (!requireNamespace("optparse", quietly = TRUE)) BiocManager::install("optparse")
library("optparse")

print("Loaded packages")

#### Parse commandline arguements ####
option_list <- list(
  optparse::make_option(c("-m", "--metblmcs_lev"), type="character", 
                        default=file.path(FALSE),
                        help="pathway_level, keyword to help find right random forest scores"),
  optparse::make_option(c("-i", "--in_dir"), type="character", 
                        default=file.path("output", "no_map_CVD", "tables"), 
                        help="dir with input 'scores' files"),
  optparse::make_option(c("-o", "--out_subdir"), type="character", 
                        default=file.path("output/no_map/graphics"), 
                        help="dataset dir path"),
  optparse::make_option(c("-p", "--out_prefix"), type="character", 
                        default=file.path("auto_prot_"), 
                        help="prefix in output file name"),
  optparse::make_option(c("-g", "--group_pattern"), type="character", 
                        default="demographics_meat", 
                        help="pattern to separate score files if there are more than one group in the dir")
);
opt_parser <- optparse::OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

print("Commandline arguments:")


df1 <- read.csv("output/no_map_auto_protein/tables/shap_feat_imp_noMap-demo-log-filt_all_bat_norm_imput-chem-auto_protein.csv",
                check.names = FALSE)

resp_vars <- c()
score <- c()
vd_list <- list()

for (rw in 1:nrow(df1)){
  resp_vars[rw] <- df1$response_var[rw]
  score[rw] <- df1$accuracy[rw]
  my_features <- df1[rw, 4:ncol(df1)]
  my_features <- my_features[order(unlist(my_features))]
  vd_list[[rw]] <- names(my_features)[1:100]
}

myCol <- brewer.pal(length(resp_vars), "Pastel2")

venn.diagram(
  x = vd_list,
  category.names = resp_vars,
  filename = file.path(opt$out_subdir, paste0(opt$out_prefix, ".png")),
  output=FALSE,
  
  # Output features
  imagetype="png",
  height = 480 ,
  width = 480 ,
  resolution = 300,
  compression = "lzw",

  # Circles
  lwd = 2,
  lty = 'blank',
  fill = myCol,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  # cat.pos = c(-27, 27, 135),
  # cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  # rotation = 1
)



