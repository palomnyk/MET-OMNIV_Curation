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
                        # default = "data/mapping/all_sites_metadata.csv",
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
                      "UARS-01-23ML+ DATA TABLES (EDTA PLASMA SAMPLES).xlsx")
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
correct_sites <- c("PSU-MED","MB/IIT","Purdue","USDA-MED")
agg_columns <- c("beef", "chicken", "pork", "turkey", "processed", "meat")
metad_intrest <- c("beef_rmOut_g", "beef_rmOut_g_per_kg_bw", "beef_rmOut_g_per_bmi", "age", "bmi", "daily_energy")#"bodyweight",
char_metad_interest <- c("SITE", "sex")
hid_leg_meta_intrst <- c("SUBJECT_ID", "TREATMENT")
hid_leg_sites <-  list(c("MB/IIT"))

#### Data reorganization ####
mtbmcs_df <- orig_mtbmcs_df[row.names(meta_df),]
stopifnot(identical(row.names(mtbmcs_df), row.names(meta_df)))
meta_df$sex[meta_df$sex == 1] <- "male"
meta_df$sex[meta_df$sex == 0] <- "female"

##### Create new column for treatment + site plot #####
meta_df$SITE_TREATMENT <- paste(meta_df$SITE, meta_df$TREATMENT, sep = ": ")

##### Create new columns for PSU/USDA Med study and MB study #####
meta_df$`USDA-PSU` <- sapply(1:nrow(meta_df), FUN = function(x){
  ifelse(meta_df$SITE[x] %in% c("USDA-MED", "PSU-MED"), 
         meta_df$SITE_TREATMENT[x], NA)})

meta_df$MB_ID <- sapply(1:nrow(meta_df), FUN = function(x){
  ifelse(meta_df$SITE[x] %in% c("PSU-MED"), 
         meta_df$SUBJECT_ID[x], NA)})

# char_metad_interest <- c(char_metad_interest, "SITE_TREATMENT")
#### PCA ####
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
numeric_plots <- lapply(1:length(metad_intrest), function(m){
  md <- metad_intrest[m]
  meta_fact <- factor(meta_df[,md])
  g <- ggplot2::ggplot(myPCA, aes(x=PC1, y=PC2)) +
    ggplot2::geom_point(aes(col = meta_df[,md])) +
    # ggplot2::geom_point(aes(shape = as.factor(meta_df$SITE), col = meta_df[,md])) +
    ggplot2::ggtitle(paste0(md)) + # Blank Title for the Graph
    ggplot2::xlab(paste0("PC1", ", ", round(my_var_exp[1],2)*100, "%")) +
    ggplot2::ylab(paste0("PC2", ", ", round(my_var_exp[2],2)*100, "%")) +
    ggplot2::labs(color = md) +
    ggplot2::theme(text=element_text(size=18)) +
    ggplot2::scale_colour_gradient(na.value = NA,
                                   low = "cornflowerblue", high = "deeppink4")
})

#### Plots for factor/character data ####
site_list <- lapply(1:length(char_metad_interest), function(m){
  md <- char_metad_interest[m]
  print(md)
  use_cols <- !is.na(meta_df[,md])
  meta_fact <- factor(meta_df[use_cols,md])
  sub_pca <- myPCA[use_cols,]
  meta_fact <- factor(meta_df[,md])
  g <- ggplot2::ggplot(myPCA, aes(x=PC1, y=PC2, col = meta_fact)) +
    # ggplot2::geom_point(aes(shape = factor(meta_df$SITE))) +
    ggplot2::geom_point() +
    ggplot2::ggtitle(paste0(md)) + # Blank Title for the Graph
    ggplot2::xlab(paste0("PC1: ", round(my_var_exp[1],2)*100, "%")) +
    ggplot2::ylab(paste0("PC2:", round(my_var_exp[2],2)*100, "%")) +
    ggplot2::labs(color = md) +
    ggplot2::theme(text=element_text(size=18)) +
    ggplot2::stat_ellipse()
})

hidden_legend <- lapply(1:length(hid_leg_meta_intrst), function(m){
  md <- hid_leg_meta_intrst[m]
  meta_fact <- factor(meta_df[,md])
  g <- ggplot2::ggplot(myPCA, aes(x=PC1, y=PC2, col = meta_fact, group=meta_fact)) +
    # ggplot2::geom_point(aes(shape = factor(meta_df$SITE))) +
    ggplot2::geom_point(show.legend = FALSE) +
    ggplot2::ggtitle(paste0(md)) + # Blank Title for the Graph
    ggplot2::xlab(paste0("PC1: ", round(my_var_exp[1],2)*100, "%")) +
    ggplot2::ylab(paste0("PC2:", round(my_var_exp[2],2)*100, "%")) +
    ggplot2::labs(color = md) +
    ggplot2::theme(text=element_text(size=18)) +
    ggplot2::stat_ellipse() +
    ggplot2::theme(legend.position = "none")
})



# sadfsd

#### Plots for individual sites ####
# for (st in 1:length(hid_leg_sites)){
hid_leg_single_site_plots <- c()
for (st in 1:length(correct_sites)) {
  mi <- correct_sites[st]
  print(mi)
  sub_meta_df <- meta_df[meta_df$SITE %in% mi, ]
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
  
  my_plots <- lapply(1:length(hid_leg_meta_intrst), function(m){
    md <- hid_leg_meta_intrst[m]
    meta_fact <- factor(sub_meta_df[,md])
    print(meta_fact)
    g <- ggplot2::ggplot(myPCA, aes(x=PC1, y=PC2, col = meta_fact, group=meta_fact)) +
      # ggplot2::geom_point(aes(shape = factor(meta_df$SITE))) +
      ggplot2::geom_point(show.legend = FALSE) +
      ggplot2::ggtitle(paste0(mi, ": ", md)) + # Blank Title for the Graph
      ggplot2::xlab(paste0("PC1: ", round(my_var_exp[1],2)*100, "%")) +
      ggplot2::ylab(paste0("PC2:", round(my_var_exp[2],2)*100, "%")) +
      ggplot2::labs(color = md) +
      ggplot2::theme(text=element_text(size=18)) +
      ggplot2::stat_ellipse() +
      ggplot2::theme(legend.position = "none")
    print(g)
    g
  })
  hid_leg_single_site_plots <- c(hid_leg_single_site_plots, my_plots)
}

#### Save plots ####
my_grobs <- c(hid_leg_single_site_plots, site_list, numeric_plots)
png(file.path(output_dir, "graphics", paste0("numeric_", opt$out_name,".png")),
    width = 25, height = 7*length(my_grobs)/8, units = "in", res = 300)


grid.arrange(grobs=my_grobs,
             ncol=4)#common.legend = TRUE, top="Gradient PCA plots"

dev.off()


print("End R script.")

