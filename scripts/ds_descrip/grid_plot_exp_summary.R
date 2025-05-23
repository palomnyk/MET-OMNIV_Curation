# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for building sankey plots showing how each response feature shares top
# X metabolomic elements by feature importance
# Left nodes = respnse feature
# Right nodes = metabolites
#

rm(list = ls()) #clear workspace

print(paste("Working in", getwd()))

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("networkD3", quietly = TRUE))  BiocManager::install("networkD3")
# Load package
library("networkD3")
if (!requireNamespace("gridExtra", quietly = TRUE))  BiocManager::install("gridExtra")
library("gridExtra")
if (!require("ggplot2")) BiocManager::install("ggplot2")
library("ggplot2")
if (!requireNamespace("optparse", quietly = TRUE)) BiocManager::install("optparse")
library("optparse")

print("Loaded dependencies")
source(file.path("scripts","data_org", "data_org_func.R"))

#### Functions ####

#### Parse command line arguments ####
option_list <- list(
  optparse::make_option(c("-f", "--input"), type="character",
                        # default="data/mapping/noMap_metadata_demo.csv",
                        default = file.path("data", "mapping", "noMap-auto_protn_metadata.csv"),
                        help="path of first csv"),
  optparse::make_option(c("-s", "--output_dir"), type="character",
                        default = "no_map", help="dir in /output"),
  optparse::make_option(c("-o", "--out_name"), type="character",
                        default = "exp_org_Sankey.html",
                        help="Path of output csv.")
);
opt_parser <- optparse::OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

print("Commandline arguments:")
print(opt)

#### Establish directory layout and other constants ####
output_dir <- file.path("output", opt$output_dir)
dir.create(output_dir)

#### Loading in data ####
inp_df <- read.csv(opt$input, check.names = FALSE,
                   row.names = "PARENT_SAMPLE_NAME")
inp_df <- inp_df[!is.na(inp_df$SITE),]
level_cols <- colnames(inp_df)[grepl("_levels",colnames(inp_df))]
meat_cols <- gsub("_levels", "", level_cols)
exp_org_cols <- c("SITE", "TREATMENT")

#### Org data ####
treats <- unique(inp_df$TREATMENT)
treat_plots <- lapply(1:length(treats), function(m){
  trmnt <- treats[m]
  
  sub_inp_df <- inp_df[inp_df$TREATMENT == trmnt, meat_cols]
  long_sub_inp_df <- reshape2::melt(sub_inp_df)
  
  if (m < length(treats)){
    g <- ggplot2::ggplot(long_sub_inp_df, aes(x=variable, y=value)) + 
      geom_boxplot() +
      ggplot2::ylab(paste(trmnt)) +
      # ggplot2::ggtitle(label = paste(trmnt)) +
      ggplot2::scale_x_discrete(guide = guide_axis(angle = 90)) +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) +
      coord_cartesian(ylim = c(0, 400))
    g
  }else{
    g <- ggplot2::ggplot(long_sub_inp_df, aes(x=variable, y=value)) + 
      geom_boxplot() +
      ggplot2::ylab(paste(trmnt)) +
      coord_cartesian(ylim = c(0, 400)) +
      theme(axis.title.x=element_blank())
      # ggplot2::ggtitle(label = paste(trmnt)) +
      # ggplot2::scale_x_discrete(guide = guide_axis(angle = 90))
    g
  }

})

# g <- ggplot2::ggplot(sub_inp_df, aes(x=variable, y=value)) + 
#   geom_boxplot() +
#   ggplot2::ylab("Grams") +
#   ggplot2::ggtitle(label = paste(treats)) +
#   ggplot2::scale_x_discrete(guide = guide_axis(angle = 90)) 
# g




pdf(file.path(output_dir, "graphics", paste0("boxplot_grid_summary.pdf")),
    width = 3.5, height = 6.5)

# for (g in treat_plots){
#   print(g)
# }

grid.arrange(arrangeGrob(grobs = treat_plots,
             ncol = 1, # Second row with 2 plots in 2 different columns
             nrow = length(treats)))
# ncol=1, nrow = length(treats)

dev.off()


print("Reached end of R script!")
