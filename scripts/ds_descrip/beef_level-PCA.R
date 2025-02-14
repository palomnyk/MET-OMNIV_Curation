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

print("Loaded packages")

#### Establish directory layout and other constants ####
metabo_f <- file.path("data", "metabolomics", "UARS-01-23ML+",
            "UARS-01-23ML+ DATA TABLES (DATA ADJUSTED BY BASELINE SAMPLES FROM EACH SITE).xlsx")
output_dir <- file.path("output", "no_map_chick_is_not_beef")
dir.create(file.path(output_dir, "graphics"))
dir.create(file.path(output_dir, "tables"))

metad_intrest <- c("beef_level", "beef_level_oz")

#### Loading in data ####
mtbmcs_df <-openxlsx::read.xlsx(metabo_f,
                        sheet = "Log-Transformed",
                        rowNames = TRUE)
meta_df <- read.csv(file = file.path("data", "mapping", "noMap_metadata.csv"),
                    header = TRUE, check.names = FALSE, row.names = 1)
chem_link <- openxlsx::read.xlsx(xlsxFile = metabo_f,
                                 sheet = "Chemical Annotation")

#### Data reorganization ####
names(mtbmcs_df) <- chem_link$CHEMICAL_NAME[match(names(mtbmcs_df), chem_link$CHEM_ID)]

mtbmcs_df <- mtbmcs_df[row.names(meta_df),]
stopifnot(identical(row.names(mtbmcs_df), row.names(meta_df)))

#### PCA ####
keep_rows <- row.names(meta_df)[which(meta_df$beef_level != "unknown")]

meta_df <- meta_df[keep_rows,]
mtbmcs_df <- mtbmcs_df[keep_rows,]

meta_df <-  type.convert(meta_df, as.is = TRUE)#Reset column types automatically (for factors)

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

meta_df$CORRECTED_SITE <- factor(meta_df$CORRECTED_SITE, levels = c(my_order))

#### Make plots ####
pdf(file.path(output_dir, "graphics", "beef_level_PCA.pdf"), width = 6, height = 4)
for(md in metad_intrest){
  # g <- ggplot2::ggplot(myPCA, aes(x=PC1, y=PC2, col = factor(meta_df[,md]))) +
  #   ggplot2::geom_point(aes(shape = factor(meta_df$CORRECTED_SITE, ))) +
  #   ggtitle(paste0("PCA of Metabolomics")) + # Blank Title for the Graph
  #   xlab(paste0("PC 1, ", round(my_var_exp[1],2)*100, "%")) +
  #   ylab(paste0("PC 2, ", round(my_var_exp[2],2)*100, "%")) +
  #   labs(color = "BEEF INTAKE", shape = "Site") +
  #   theme(text=element_text(size=18))
  # print(g)
  meta_fact <- factor(meta_df[,md])
  t_pval_1 <- summary(aov(myPCA$PC1~meta_fact))[[1]][["Pr(>F)"]][[1]]#do anova and extract pvalue
  print(paste("PVAL PC1: ", t_pval_1))
  t_pval_2 <- summary(aov(myPCA$PC2~meta_fact))[[1]][["Pr(>F)"]][[1]]#do anova and extract pvalue
  print(paste("PVAL PC2: ", t_pval_2))
  pc1_symb <- ifelse( t_pval_1 < 0.05, yes = "*", no = "")
  pc2_symb <- ifelse( t_pval_2 < 0.05, yes = "*", no = "")
  g <- ggplot2::ggplot(myPCA, aes(x=PC1, y=PC2, col = meta_fact, group=meta_fact)) +
    ggplot2::geom_point(aes(shape = factor(meta_df$CORRECTED_SITE))) +
    # ggplot2::ggtitle(paste0("PCA of Metabolomics")) + # Blank Title for the Graph
    ggplot2::xlab(paste0("PC1", pc1_symb, ", ", round(my_var_exp[1],2)*100, "%")) +
    ggplot2::ylab(paste0("PC2", pc2_symb, ", ", round(my_var_exp[2],2)*100, "%")) +
    ggplot2::labs(color = md, shape = "Site") +
    ggplot2::theme(text=element_text(size=18)) +
    stat_ellipse()
  ggplot2::ggsave(g, device = "png",
                  filename = file.path(output_dir, "graphics", paste0(md, "_PCA_elipse.png")))
  print(g)
  
  g <- ggplot2::ggplot(myPCA, aes(x=PC1, y=PC2, col = meta_fact, group=meta_fact)) +
    ggplot2::geom_point(aes(shape = factor(meta_df$CORRECTED_SITE))) +
    # ggplot2::ggtitle(paste0("PCA of Metabolomics")) + # Blank Title for the Graph
    ggplot2::xlab(paste0("PC1", pc1_symb, ", ", round(my_var_exp[1],2)*100, "%")) +
    ggplot2::ylab(paste0("PC2", pc2_symb, ", ", round(my_var_exp[2],2)*100, "%")) +
    ggplot2::labs(color = md, shape = "Site") +
    ggplot2::theme(text=element_text(size=18)) +
  ggplot2::ggsave(g, device = "png",
                  filename = file.path(output_dir, "graphics", paste0(md, "_PCA.png")))
  print(g)
  
  g <- ggplot2::ggplot(myPCA, aes(x=PC1, y=meta_fact)) +
    ggplot2::geom_boxplot() +
    ggplot2::ggtitle(paste0("Boxplot of PC1")) + # Blank Title for the Graph
    ggplot2::xlab(paste0("PC1", pc1_symb)) +
    ggplot2::ylab(paste0(md)) +
    # ggplot2::labs(color = md, shape = "Site") +
    ggplot2::theme(text=element_text(size=18))
  print(g)
  
  g <- ggplot2::ggplot(myPCA, aes(x=PC2, y=meta_fact)) +
    ggplot2::geom_boxplot() +
    ggplot2::ggtitle(paste0("Boxplot of PC2")) + # Blank Title for the Graph
    ggplot2::xlab(paste0("PC2", pc1_symb)) +
    ggplot2::ylab(paste0(md)) +
    # ggplot2::labs(color = md, shape = "Site") +
    ggplot2::theme(text=element_text(size=18))
  print(g)
  
}

g <- ggplot2::ggplot(myPCA, aes(x=PC1, y=PC2, col = meta_df[,"beef_level_oz"])) +
  ggplot2::geom_point(aes(shape = factor(meta_df$CORRECTED_SITE))) +
  # ggplot2::ggtitle(paste0("PCA of Metabolomics")) + # Blank Title for the Graph
  ggplot2::xlab(paste0("PC1", pc1_symb, ", ", round(my_var_exp[1],2)*100, "%")) +
  ggplot2::ylab(paste0("PC2", pc2_symb, ", ", round(my_var_exp[2],2)*100, "%")) +
  ggplot2::labs(color = md, shape = "Site") +
  ggplot2::theme(text=element_text(size=18)) +
  ggplot2::scale_colour_gradient(low = "cornflowerblue", high = "deeppink4", na.value = NA)
  # ggplot2::scale_colour_gradient(colors = heat.colors(2))
  # heat.colors(n, alpha, rev = FALSE)
  print(g)

dev.off()

print("End R script.")

