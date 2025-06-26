# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for making PCA of all the meats in numeric form

rm(list = ls()) #clear workspace

print(paste("Working in", getwd()))

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("openxlsx", quietly = TRUE))  BiocManager::install("openxlsx")
library("openxlsx")
if (!require("ggplot2")) BiocManager::install("ggplot2")
library("ggplot2")
if (!requireNamespace("optparse", quietly = TRUE)) BiocManager::install("optparse")
library("optparse")
if (!requireNamespace("gridExtra", quietly = TRUE))  BiocManager::install("gridExtra")
library("gridExtra")
print("Loaded packages")

#### Parse command line arguments ####
option_list <- list(
  optparse::make_option(c("-m", "--matad_path"), type="character",
                        # default = "data/mapping/noMap_metadata.csv",
                        default = "data/diet/nutrition_data/all_sites-meats_normalize_full_df.csv",
                        help="data from which colors and symbols will come"),
  optparse::make_option(c("-p", "--points"), type="character",
                        # default = "data/metabolomics/filt_all_bat_norm_imput-chem.csv",
                        default = "data/metabolomics/demo-log-filt_all_bat_norm_imput-chem.csv",
                        help="data from which colors and symbols will come"),
  optparse::make_option(c("-s", "--output_dir"), type="character",
                        default = "no_map", help="dir in /output"),
  optparse::make_option(c("-o", "--out_name"), type="character",
                        default = "metabolite_pca",
                        help="main output label without prefixes")
);
opt_parser <- optparse::OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

print("Commandline arguments:")
print(opt)

#### Establish directory layout and other constants ####
metabo_f <- file.path("data", "metabolomics", "UARS-01-23ML+",
            "UARS-01-23ML+ DATA TABLES (DATA ADJUSTED BY BASELINE SAMPLES FROM EACH SITE).xlsx")
output_dir <- file.path("output", opt$output_dir)
dir.create(file.path(output_dir, "graphics"))
dir.create(file.path(output_dir, "tables"))

#### Loading in data ####
orig_mtbmcs_df <- read.csv(file = file.path(opt$points),
                      header = TRUE, check.names = FALSE, row.names = "PARENT_SAMPLE_NAME")
meta_df <- read.csv(file = file.path(opt$matad_path),
                    header = TRUE, check.names = FALSE, row.names = 1)
chem_link <- openxlsx::read.xlsx(xlsxFile = metabo_f,
                                 sheet = "Chemical Annotation")

#### Establish variables ####
correct_sites <- c("PSU-MED","MB/IIT","Purdue","USDA-MAP","USDA-MED")
agg_columns <- c("beef", "chicken", "pork", "turkey", "processed", "meat")

# meta_df[,paste0(ac,"_g")] <- meta_df[,ac]
# meta_df[,paste0(ac,"_g_per_kg_bw")] <- meta_df[,ac]

#### Data reorganization ####
mtbmcs_df <- orig_mtbmcs_df[row.names(meta_df),]
stopifnot(identical(row.names(mtbmcs_df), row.names(meta_df)))

#### PCA ####
metad_intrest <- colnames(meta_df)

print("Checking for columns/chems with zero variance")

zero_var <- which(apply(mtbmcs_df, 2, var) == 0)

print(paste("Num with zero var:", length(zero_var)))
print("removing the following:")
print(paste(names(zero_var), collapse = ", "))
mtbmcs_df <- mtbmcs_df[, which(apply(mtbmcs_df, 2, var) != 0)]

#create PCA
my_prcmp <- prcomp(na.omit(mtbmcs_df), 
                   center = TRUE,
                   scale = TRUE)

#extract PCA matrix and convert to dataframe
myPCA <- data.frame(my_prcmp$x)

print("Creating proportion of variance explained")
my_var_exp <- my_prcmp$sdev^2/sum(my_prcmp$sdev^2)

my_order <- c("USDA-MED", "PSU-MED", "Purdue", "MB/IIT")

meta_df$SITE <- factor(meta_df$SITE, levels = c(my_order))

#### Make gradient plots for numeric data ####
numeric_cols_TF <- unlist(lapply(meta_df, is.numeric), use.names = TRUE)#find numeric columns
metad_intrest <- paste0(agg_columns ,"_g_per_kg_bw")

numeric_plots <- vector(mode = "list", length = length(metad_intrest))

test <- meta_df[meta_df$CLIENT_IDENTIFIER == "MED 232 DP2.2 EDTA Plas"]

interest_labels <- c("MED 232 DP2.2 EDTA Plas", "5404 B4", "MED 230 DP3.2 EDTA Plas", "5407 B4",
  "MED 207 DP1.2 EDTA Plas", "5426 B8", "MED 218 DP3.2 EDTA Plas", "5422 B8")

numeric_plots <- lapply(1:length(metad_intrest), function(m){
  md <- metad_intrest[m]
  meta_fact <- factor(meta_df[,md])
  g <- ggplot2::ggplot(myPCA, aes(x=PC1, y=PC2)) +
    # ggplot2::geom_point(aes(col = meta_df[,md])) +
    ggplot2::geom_point(aes(shape = factor(meta_df$SITE), col = meta_df[,md])) +
    ggplot2::ggtitle(paste0( md)) + # Blank Title for the Graph
    ggplot2::xlab(paste0("PC1", ", ", round(my_var_exp[1],2)*100, "%")) +
    ggplot2::ylab(paste0("PC2", ", ", round(my_var_exp[2],2)*100, "%")) +
    ggplot2::labs(color = md, shape = "SITE") +
    ggplot2::theme(text=element_text(size=18)) +
    ggplot2::scale_colour_gradient(low = "cornflowerblue", high = "deeppink4", na.value = NA)
})

site_list <- lapply(1:1, function(m){
g <- ggplot2::ggplot(myPCA, aes(x=PC1, y=PC2, col = meta_df$SITE)) +
  # ggplot2::geom_point(aes(shape = factor(meta_df$SITE))) +
  ggplot2::geom_point() +
  ggplot2::ggtitle(paste0("SITE")) + # Blank Title for the Graph
  ggplot2::xlab(paste0("PC1: ", round(my_var_exp[1],2)*100, "%")) +
  ggplot2::ylab(paste0("PC2:", round(my_var_exp[2],2)*100, "%")) +
  ggplot2::labs(color = "SITE") +
  ggplot2::theme(text=element_text(size=18)) +
  ggplot2::stat_ellipse()
})
# pdf(file.path(output_dir, "graphics", paste0("numeric_", opt$out_name,".pdf")),
#     width = 22, height = 10)


grid.arrange(grobs=c(site_list, numeric_plots),
             ncol=4,
             common.legend = TRUE, top="Gradient PCA plots")

# dev.off()


#### Plots for individual sites ####
for (st in unique(meta_df$SITE)){
  print(st)
  sub_meta_df <- meta_df[meta_df$SITE == st, ]
  sub_mtbmcs_df <- orig_mtbmcs_df[row.names(sub_meta_df),]
  
  print("Checking for columns/chems with zero variance")
  
  zero_var <- which(apply(sub_mtbmcs_df, 2, var) == 0)
  
  print(paste("Num with zero var:", length(zero_var)))
  print("removing the following:")
  print(paste(names(zero_var), collapse = ", "))
  sub_mtbmcs_df <- sub_mtbmcs_df[, which(apply(sub_mtbmcs_df, 2, var) != 0)]
  #create PCA
  my_prcmp <- prcomp(na.omit(sub_mtbmcs_df), 
                     center = TRUE)
  
  #extract PCA matrix and convert to dataframe
  myPCA <- data.frame(my_prcmp$x)
  
  print("Creating proportion of variance explained")
  my_var_exp <- my_prcmp$sdev^2/sum(my_prcmp$sdev^2)
  
  #### Make gradient plots for numeric data ####
  metad_intrest <- paste0(agg_columns ,"_g_per_kg_bw")
  
  for (m in 1:length(metad_intrest)){
    md <- metad_intrest[m]
    print(paste(st, md))
    meta_fact <- factor(sub_meta_df[,md])
    g <- ggplot2::ggplot(myPCA, aes(x=PC1, y=PC2)) +
      # ggplot2::geom_point(aes(col = meta_df[,md])) +
      ggplot2::geom_point(aes( col = sub_meta_df[,md])) +#shape = factor(meta_df$SITE),
      ggplot2::ggtitle(paste(st, md)) + # Blank Title for the Graph
      ggplot2::xlab(paste0("PC1", ", ", round(my_var_exp[1],2)*100, "%")) +
      ggplot2::ylab(paste0("PC2", ", ", round(my_var_exp[2],2)*100, "%")) +
      ggplot2::labs(color = md, shape = "SITE") +
      ggplot2::theme(text=element_text(size=18)) +
      ggplot2::scale_colour_gradient(low = "cornflowerblue", high = "deeppink4", na.value = NA)
    print(g)
  }
}


print("End R script.")

