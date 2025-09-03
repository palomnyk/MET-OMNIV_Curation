# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for making Venn diagrams to compare feature importances from 
# random forest

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
                        default="chem",
                        help="pathway_level, keyword to help find right random forest scores"),
  optparse::make_option(c("-i", "--infile"), type="character", 
                        default="reorg-Venn-{resp_var}-shap_feat_imp-demo-log-filt_all_bat_norm_imput-chem-meats_g.c-{metblmcs_lev}", 
                        help="file input feature importance"),
  optparse::make_option(c("-o", "--outname"), type="character", 
                        default=file.path("output/no_map/graphics/Venn.png"),
                        help="output file name"),
  optparse::make_option(c("-g", "--group_column"), type="character", 
                        default="response_var", 
                        help="which column contains the names of the groups that we want to compare"),
  optparse::make_option(c("-x", "--top_X"), type="integer",
                        default = 100,
                        help="Number of comparison features in plot.")
);
opt_parser <- optparse::OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

print("Commandline arguments:")
print(opt)

df1 <- read.csv(opt$infile,
                check.names = FALSE)

resp_vars <- c()
score <- c()
vd_list <- list()

print(names(df1)[1:10])
for (rw in 1:nrow(df1)){
  resp_vars[rw] <- df1[rw,opt$group_column]
  score[rw] <- df1$accuracy[rw]
  my_features <- df1[rw, 4:ncol(df1)]
  my_features <- my_features[order(unlist(my_features))]
  vd_list[[rw]] <- names(my_features)[1:opt$top_X]
}

paste("brewer.pal")
print(length(resp_vars))
myCol <- brewer.pal(length(resp_vars), "Pastel2")

venn.diagram(
  x = vd_list,
  category.names = resp_vars,
  filename = file.path(opt$outname),
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

print("End R script")