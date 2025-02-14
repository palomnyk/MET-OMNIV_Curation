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
library("networkD3")
if (!requireNamespace("optparse", quietly = TRUE)) BiocManager::install("optparse")
library("optparse")

print("Loaded dependencies")
source(file.path("scripts","data_org", "data_org_func.R"))

#### Functions ####

#### Parse command line arguments ####
option_list <- list(
  optparse::make_option(c("-f", "--featimp_path"), type="character",
                        default = "output/no_map_chick_is_not_beef/tables/feat_imp_demo-log-filt_all_bat_norm_imput-sub_pathway.csv",
                        # default = "output/no_map_chick_is_not_beef/tables/feat_imp_demo-log-filt_all_bat_norm_imput-chem.csv",
                        help="path of first csv"),
  optparse::make_option(c("-s", "--output_dir"), type="character",
                        default = "no_map", help="dir in /output"),
  optparse::make_option(c("-o", "--out_name"), type="character",
                        default = "metabolites_Sankey.html",
                        help="Path of output csv.")
);
opt_parser <- optparse::OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

print("Commandline arguments:")
print(opt)

#### Establish directory layout and other constants ####
output_dir <- file.path("output", opt$output_dir)
dir.create(output_dir)

accuracy_threshold <- 0.05
top_X <- 5

#### Loading in data ####
feat_imp <- read.csv(opt$featimp_path, check.names = FALSE)

#### Org data ####
ave_accuracies <- aggregate(feat_imp$accuracy, by=list(feat_imp$response_var), mean)
#remove features below threshold
ave_accuracies <- ave_accuracies[ave_accuracies$x > accuracy_threshold, ]

#empty variables to fill in
link_source <- character(0)
link_target <- character(0)
name_source <- character(0)
name_target <- character(0)

#### Find top x elements for each resp feature, make links ####
for (r in 1:nrow(ave_accuracies)){
  rv <- ave_accuracies$Group.1[r]
  df <- feat_imp[feat_imp$response_var == rv, 4:ncol(feat_imp)]
  ele_means <- colMeans(df)
  top_ele <- sort(ele_means, decreasing = TRUE)[1:top_X]
  for (ele in 1:top_X){
    el <- top_ele[ele]
    print(el)
    name_source <- c(name_source, rv)
    name_target <- c(name_target, names(el)[1])
  }
}

node_names <- c(name_source, name_target)
unique_nodes <- unique(node_names)

target_index <- match(name_target, unique_nodes)
target_index <- target_index - 1
source_index <- match(name_source, unique_nodes)
source_index <- source_index - 1

links <- data.frame(source = source_index, target = target_index, 
                    count = rep(1, length(target_index)))
nodes <- data.frame(nodes = unique_nodes)


#### Make plot ####
p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                   Target = "target", Value = "count", NodeID = "nodes",
                   units = "Studies", fontSize = 14, nodeWidth = 30, 
                   margin = list(50,100,500,100), sinksRight = TRUE,
                   height = 500, width = 500)
p

p <- htmlwidgets::prependContent(p, htmltools::tags$h1("Figure . Sankey plot showing metabolites that are important to each feature."))
# p <- htmlwidgets::appendContent(p, htmltools::tags$p("*, SR that included children/adolescents only; ~, SR that included only adults; all other SR included all age groups."))
# p <- htmlwidgets::appendContent(p, htmltools::tags$p("Higgins KA, Rawal R, Kramer M, Baer DJ, Yerke A, Klurfeld DM. An overview of reviews on the association of low calorie sweetener consumption with body weight and adiposity. Advances in Nutrition (accepted)."))

saveNetwork(p, file=file.path(output_dir, "graphics", opt$out_name), selfcontained = TRUE)
