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
print("Loaded packages")

if (!requireNamespace("optparse", quietly = TRUE)) BiocManager::install("optparse")
library("optparse")

print("Loaded dependencies")
source(file.path("scripts","data_org", "data_org_func.R"))

#### Functions ####

#### Parse command line arguments ####
option_list <- list(
  optparse::make_option(c("-f", "--input"), type="character",
                        # default="data/mapping/noMap_metadata_demo.csv",
                        default = file.path("data", "mapping", "noMap_auto_protn_metadata.csv"),
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

accuracy_threshold <- 0.1
top_X <- 5

#### Loading in data ####
inp_df <- read.csv(opt$input, check.names = FALSE,
                     row.names = "PARENT_SAMPLE_NAME")
inp_df <- inp_df[!is.na(inp_df$SITE),]
level_cols <- colnames(inp_df)[grepl("_level",colnames(inp_df))]
exp_org_cols <- c("SITE", "TREATMENT")

#### Org data ####

#empty variables to fill in
name_source <- character(0)
name_target <- character(0)
counts <- c()

# Find top x elements for each exp feature, make links
for (exp in 1:length(exp_org_cols)-1){
  my_col <- exp_org_cols[exp]
  print(my_col)
  column_1 <- inp_df[,my_col]
  my_col2 <- exp_org_cols[exp+1]
  column_2 <- inp_df[,my_col2]
  for (link_start in unique(column_1)){
    for(link_target in unique(column_2)){
      count <- sum(inp_df[,my_col] == link_start &
                     inp_df[,my_col2] == link_target)
      if (count > 0){
        col_count1 <- sum(column_1 == link_start)
        col_count2 <- sum(column_2 == link_target)
        name_source <- c(name_source, paste(c(link_start, col_count1), collapse = ": "))
        name_target <- c(name_target, paste(c(link_target, col_count2), collapse = ": "))
        counts <- c(counts, count)
      }
    }
  }
}


# Find top x elements for each each feature, make links
# my_col <- exp_org_cols[2]
# column_1 <- inp_df[,exp_org_cols[2]]
# for (exp in 1:length(level_cols)){
#   my_col2 <- level_cols[exp]
#   column_2 <- inp_df[,my_col2]
#   for (link_start in unique(column_1)){
#     for(link_target in unique(column_2)){
#       count <- sum(inp_df[,my_col] == link_start &
#                    inp_df[,my_col2] == link_target )
#       print(link_start)
#       print(link_target)
#       print(count)
#       if (count > 0){
#         print("Count greater than 0")
#         col_count1 <- sum(column_1 == link_start)
#         col_count2 <- sum(column_2 == link_target)
#         name_source <- c(name_source, paste(c(link_start, col_count1), collapse = ": "))
#         name_target <- c(name_target, paste(c(my_col2, link_target), collapse = ": "))
#         counts <- c(counts, count)
#       }
#     }
#   }
# }

node_names <- c(name_source, name_target)
unique_nodes <- unique(node_names)

target_index <- match(name_target, unique_nodes)
target_index <- target_index - 1
source_index <- match(name_source, unique_nodes)
source_index <- source_index - 1


links <- data.frame(source = source_index, target = target_index, 
                    count = counts)
nodes <- data.frame(nodes = unique_nodes)

#update node names to clarify in plot
p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                   Target = "target", Value = "count", NodeID = "nodes",
                   units = " participants", fontSize = 14, nodeWidth = 30, 
                   margin = list(50,100,500,100), sinksRight = TRUE,
                   height = 500, width = 300)
p

# p <- htmlwidgets::prependContent(p, htmltools::tags$h1("Figure . Sankey plot showing metabolites that are important to each feature."))
# # p <- htmlwidgets::appendContent(p, htmltools::tags$p("*, SR that included children/adolescents only; ~, SR that included only adults; all other SR included all age groups."))
# # p <- htmlwidgets::appendContent(p, htmltools::tags$p("Higgins KA, Rawal R, Kramer M, Baer DJ, Yerke A, Klurfeld DM. An overview of reviews on the association of low calorie sweetener consumption with body weight and adiposity. Advances in Nutrition (accepted)."))
# 
saveNetwork(p, file=file.path(output_dir, "graphics", opt$out_name), selfcontained = TRUE)

print("End R script.")
