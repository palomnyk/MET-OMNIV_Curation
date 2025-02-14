# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for making PCA

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
                        default = "data/mapping/rf_noMap_auto_protn_metadata.csv",
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
mtbmcs_df <-openxlsx::read.xlsx(metabo_f,
                        sheet = "Log-Transformed",
                        rowNames = TRUE)
meta_df <- read.csv(file = file.path(opt$matad_path),
                    header = TRUE, check.names = FALSE, row.names = 1)
chem_link <- openxlsx::read.xlsx(xlsxFile = metabo_f,
                                 sheet = "Chemical Annotation")

#### Data reorganization ####
names(mtbmcs_df) <- chem_link$CHEMICAL_NAME[match(names(mtbmcs_df), chem_link$CHEM_ID)]

mtbmcs_df <- mtbmcs_df[row.names(meta_df),]
stopifnot(identical(row.names(mtbmcs_df), row.names(meta_df)))

# keep_rows <- row.names(meta_df)[which(meta_df$beef != "unknown")]
# 
# #remove rows with NA
# meta_df <- meta_df[keep_rows,]
# mtbmcs_df <- mtbmcs_df[keep_rows,]
# meta_df <-  type.convert(meta_df, as.is = TRUE)#Reset column types automatically (for factors)

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
numeric_cols_TF <- unlist(lapply(meta_df, is.numeric), use.names = TRUE)
metad_intrest <- names(meta_df)[numeric_cols_TF]
numeric_plots <- vector(mode = "list", length = length(metad_intrest))

numeric_plots <- lapply(1:length(metad_intrest), function(m){
  md <- metad_intrest[m]
  meta_fact <- factor(meta_df[,md])
  g <- ggplot2::ggplot(myPCA, aes(x=PC1, y=PC2)) +
    ggplot2::geom_point(aes(col = meta_df[,md])) +
    # ggplot2::geom_point(aes(shape = factor(meta_df$SITE))) +
    ggplot2::ggtitle(paste0( md)) + # Blank Title for the Graph
    ggplot2::xlab(paste0("PC1", ", ", round(my_var_exp[1],2)*100, "%")) +
    ggplot2::ylab(paste0("PC2", ", ", round(my_var_exp[2],2)*100, "%")) +
    ggplot2::labs(color = md) +
    ggplot2::theme(text=element_text(size=18)) +
    ggplot2::scale_colour_gradient(low = "cornflowerblue", high = "deeppink4", na.value = NA)
})

pdf(file.path(output_dir, "graphics", paste0("numeric_", opt$out_name,".pdf")),
    width = 36, height = 8)

grid.arrange(grobs=numeric_plots,ncol=length(metad_intrest),
             common.legend = TRUE, top="Gradient PCA plots")

dev.off()

#### Make plots of meat_levels ####
metad_intrest <- names(meta_df)[!numeric_cols_TF]
##### Get pc1 and pc2 pvalues for pvalue correction #####
pc1_pvals <- c()
pc2_pvals <- c()
for(m in 1:length(metad_intrest)){
  md <- metad_intrest[m]
  meta_fact <- factor(meta_df[,md])
  print(levels(meta_fact))
  # anova_pval_1 <- summary(aov(myPCA$PC1~meta_fact))[[1]][["Pr(>F)"]][[1]]#do anova and extract pvalue
  anova_pval_1 <- kruskal.test(myPCA$PC1~meta_fact)$p.value
  pc1_pvals <- c(pc1_pvals, anova_pval_1)
  anova_pval_2 <- kruskal.test(myPCA$PC2~meta_fact)$p.value
  pc2_pvals <- c(pc1_pvals, anova_pval_2)
}
pc1_adj_pv <- p.adjust(pc1_pvals, method = "BH")
pc2_adj_pv <- p.adjust(pc2_pvals, method = "BH")

##### Loop through and make plots #####
factor_plots <- lapply(1:length(metad_intrest), function(m){
  md <- metad_intrest[m]
  meta_fact <- factor(meta_df[,md])
  pc1_adj <- pc1_adj_pv[m]
  pc2_adj <- pc2_adj_pv[m]
  pc1_symb <- ifelse( pc1_adj < 0.05, yes = "*", no = "")
  pc2_symb <- ifelse( pc2_adj < 0.05, yes = "*", no = "")
  g <- ggplot2::ggplot(myPCA, aes(x=PC1, y=PC2, col = meta_fact, group=meta_fact)) +
    # ggplot2::geom_point(aes(shape = factor(meta_df$SITE))) +
    ggplot2::geom_point() +
    ggplot2::ggtitle(paste0(md)) + # Blank Title for the Graph
    ggplot2::xlab(paste0("PC1", pc1_symb, ", ", round(my_var_exp[1],2)*100, "%")) +
    ggplot2::ylab(paste0("PC2", pc2_symb, ", ", round(my_var_exp[2],2)*100, "%")) +
    ggplot2::labs(color = md, shape = "Site") +
    ggplot2::theme(text=element_text(size=18)) +
    stat_ellipse()
})

pc1_plots <- lapply(1:length(metad_intrest), function(m){
  md <- metad_intrest[m]
  meta_fact <- factor(meta_df[,md])
  pc1_adj <- pc1_adj_pv[m]
  pc2_adj <- pc2_adj_pv[m]
  pc1_symb <- ifelse( pc1_adj < 0.05, yes = "*", no = "")
  pc2_symb <- ifelse( pc2_adj < 0.05, yes = "*", no = "")
  g <- ggplot2::ggplot(myPCA, aes(x=PC1, y=meta_fact)) +
    ggplot2::geom_boxplot() +
    ggplot2::ggtitle(paste0(md, " PC1")) + # Blank Title for the Graph
    ggplot2::xlab(paste0("PC1", pc1_symb)) +
    ggplot2::ylab(paste0(md)) +
    # ggplot2::labs(color = md, shape = "Site") +
    ggplot2::theme(text=element_text(size=18))
})

pc2_plots <- lapply(1:length(metad_intrest), function(m){
  md <- metad_intrest[m]
  meta_fact <- factor(meta_df[,md])
  pc1_adj <- pc1_adj_pv[m]
  pc2_adj <- pc2_adj_pv[m]
  pc1_symb <- ifelse( pc1_adj < 0.05, yes = "*", no = "")
  pc2_symb <- ifelse( pc2_adj < 0.05, yes = "*", no = "")
  g <- ggplot2::ggplot(myPCA, aes(x=PC2, y=meta_fact)) +
    ggplot2::geom_boxplot() +
    ggplot2::ggtitle(paste0(md, " PC2")) +
    ggplot2::xlab(paste0("PC2", pc2_symb)) +
    ggplot2::ylab(paste0(md)) +
    # ggplot2::labs(color = md, shape = "Site") +
    ggplot2::theme(text=element_text(size=18))
})

pdf(file.path(output_dir, "graphics", paste0("factor_", opt$out_name,".pdf")),
    width = 25, height = 2.5*9)

factor_plots <- c(factor_plots, pc1_plots, pc2_plots)
lay <- rbind(c(1,1,10,6,6,15),
             c(1,1,19,6,6,24),
             c(2,2,11,7,7,16),
             c(2,2,20,7,7,25),
             c(3,3,12,8,8,17),
             c(3,3,21,8,8,26),
             c(4,4,13,9,9,18),
             c(4,4,22,9,9,27),
             c(5,5,14,NA,NA,NA),
             c(5,5,23,NA,NA,NA))
               
grid.arrange(grobs=factor_plots, layout_matrix = lay,
             top="Categorized PCA plots")

dev.off()

print("End R script.")

