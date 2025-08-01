# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for building Sankey plots showing how each response feature shares top
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
                        # default="data/mapping/all_sites_metadata_demo.csv",
                        default = file.path("data", "diet", "nutrition_data", "all_sites-meats_normalize_full_df.csv"),
                        help="path of first csv"),
  optparse::make_option(c("-s", "--output_dir"), type="character",
                        default = "no_map", help="dir in /output"),
  optparse::make_option(c("-o", "--out_name"), type="character",
                        default = "boxplot_grid_summary_meats_g.pdf",
                        help="Path of output csv."),
  optparse::make_option(c("-u", "--suffix"), type="character",
                        # default = "_g_per_bmi",
                        # default = "_g_per_kg_bw",
                        default = "_g",
                        help="Suffix of columns wanted")
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
my_reg_ex <- paste0(opt$suffix, "$")
g_cols <- colnames(inp_df)[grepl(my_reg_ex,colnames(inp_df))]
g_cols <- g_cols[!grepl("rmOut",g_cols)]
exp_org_cols <- c("SITE", "TREATMENT")
max_y <- max(inp_df[,g_cols], na.rm = TRUE) * 1.05

#### Org data and plot ####
treats <- unique(inp_df$TREATMENT)
treat_plots <- lapply(1:length(treats), function(m){
  trmnt <- treats[m]
  print("treatment")
  print(trmnt)
  # Subset data by treatment
  sub_inp_df <- inp_df[inp_df$TREATMENT == trmnt, g_cols]
  # Create long version of dataframe
  long_sub_inp_df <- reshape2::melt(sub_inp_df)
  long_sub_inp_df[is.na(long_sub_inp_df)] <- 0
  
  # Make colors: red if boxplot is empty, black if it is >0
  my_colors <- c()
  my_counts <- c()
  for (v in unique(long_sub_inp_df$variable)){
    my_subset <- long_sub_inp_df[long_sub_inp_df$variable == v,"value"]
    if (all(unique(my_subset[!is.na(my_subset)]) %in% c(0)) || all(is.na(my_subset))){
      my_colors <- c(my_colors,"red")
    }else{
      my_colors <- c(my_colors,"black")
    }
    my_counts <- c(my_counts, length(my_subset[my_subset != 0]))
  }
  
  print(my_counts)
  print(length(my_counts))
  
  # Make vector holding number of counts in each boxplot
  

  if (m < length(treats)){
    g <- ggplot2::ggplot(long_sub_inp_df, aes(x=variable, y=value)) + 
      geom_boxplot(color = my_colors) +
      ggplot2::ylab(paste(trmnt)) +
      # ggplot2::ggtitle(label = paste(trmnt)) +
      ggplot2::annotate("text",
               x = 1:length(my_counts),
               y = aggregate(value ~ variable, long_sub_inp_df, median)[ , 2],
               # y = rep(max_y * 0.95, length(my_counts)),
               label = my_counts,
               col = "blue",
               vjust = - 1) +
      ggplot2::scale_x_discrete(guide = guide_axis(angle = 90)) +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) +
      scale_color_manual(values=my_colors) +
      coord_cartesian(ylim = c(0, max_y))
    g
  }else{
    g <- ggplot2::ggplot(long_sub_inp_df, aes(x=variable, y=value)) + 
      geom_boxplot(color = unlist(my_colors)) +
      ggplot2::ylab(paste(trmnt)) +
      coord_cartesian(ylim = c(0, max_y)) +
      theme(axis.title.x=element_blank()) +
      ggplot2::annotate("text",
                        x = 1:length(my_counts),
                        y = aggregate(value ~ variable, long_sub_inp_df, median)[ , 2],
                        # y = rep(max_y * 0.95, length(my_counts)),
                        label = my_counts,
                        col = "blue",
                        vjust = - 1) 
      # ggplot2::ggtitle(label = paste(trmnt)) +
      # ggplot2::scale_x_discrete(guide = guide_axis(angle = 90))
    g
  }
})

#### Arrange plots and save output ####

pdf(file.path(output_dir, "graphics", opt$out_name),
    width = 24, height = 10)

grid.arrange(arrangeGrob(grobs = treat_plots,
             ncol = 1, # Second row with 2 plots in 2 different columns
             nrow = length(treats)))

dev.off()

print("Reached end of R script!")

