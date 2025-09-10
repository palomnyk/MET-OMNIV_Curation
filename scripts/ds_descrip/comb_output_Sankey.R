# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for building Sankey plots showing how each response feature shares top
# X metabolomic elements by feature importance
# Left nodes = response feature
# Right nodes = metabolites

rm(list = ls()) #clear workspace

print(paste("Working in", getwd()))

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("networkD3", quietly = TRUE))  BiocManager::install("networkD3")
library("networkD3")
if (!requireNamespace("optparse", quietly = TRUE)) BiocManager::install("optparse")
library("optparse")
if (!requireNamespace("pandoc", quietly = TRUE)) BiocManager::install("pandoc")
library("pandoc")
if (!requireNamespace("webshot", quietly = TRUE)) BiocManager::install("webshot")
library("webshot")

print("Loaded dependencies")
source(file.path("scripts","data_org", "data_org_func.R"))

#### Functions ####

#### Parse command line arguments ####
option_list <- list(
  optparse::make_option(c("-f", "--featimp_path"), type="character",
                        default = "output/norm_meat/tables/shap_feat_imp_MB_IIT-demo-log-filt_all_bat_norm_imput-chem-meats_rmOut_g_per_kg_bw.csv",
                        # default = "output/ml_eval/tables/reorg-chicken-shap_feat_imp-demo-log-filt_all_bat_norm_imput-chem-.csv",
                        help="path of first csv"),
  optparse::make_option(c("-s", "--output_dir"), type="character",
                        default = "no_map", help="dir in /output"),
  optparse::make_option(c("-o", "--out_name"), type="character",
                        default = "output/no_map/graphics/test_metabolites_Sankey.html",
                        help="Path of output csv."),
  optparse::make_option(c("-t", "--accuracy_threshold"), type="numeric",
                        default = -1000000000,
                        help="Threshold for score for inclusion in graphic."),
  optparse::make_option(c("-g", "--grouping_column"), type="character",
                        default = "response_var",
                        # default = "SITE",
                        help="Column for grouping - not features"),
  optparse::make_option(c("-x", "--top_X"), type="integer",
                        default = 5,
                        help="Number of comparison features in plot."),
  optparse::make_option(c("-r", "--group_subset"), type="character",
                        default = FALSE,
                        help="Array of acceptable grouping column variables as 
                        coma seperated string. Use if wanting to use a subset of 
                        the available grouping column variables. Ex:
                        'beef_rmOut_g_per_kg_bw,chicken_rmOut_g_per_kg_bw,pork_rmOut_g_per_kg_bw,turkey_rmOut_g_per_kg_bw,meat_rmOut_g_per_kg_bw'"),
  optparse::make_option(c("-e", "--plot_height"), type="integer",
                        default = 500,
                        help="Height of Sankey plot")
);
opt_parser <- optparse::OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

print("Commandline arguments:")
print(opt)

#### Establish directory layout and other constants ####
output_dir <- file.path("output", opt$output_dir)
dir.create(output_dir)

plain_resp_vars <- c("beef","chicken","pork","turkey","meat")
all_proc <- c("beef_proc","chicken_proc","pork_proc", "turkey_proc","meat_proc")
all_mproc <- c("beef_min_proc","chicken_min_proc","pork_min_proc", "turkey_min_proc","meat_min_proc")

accuracy_threshold <- opt$accuracy_threshold
top_X <- opt$top_X

#### Loading in data ####
feat_imp <- read.csv(opt$featimp_path, check.names = FALSE)
print(head(feat_imp))

#### Org data ####
##### Subset grouping column if needed ####
if (opt$group_subset != FALSE){
  my_subset <- unlist(strsplit(opt$group_subset, split = ","))
  feat_imp <- feat_imp[feat_imp[,opt$grouping_column] %in% my_subset, ]
}

# Aggregate accuracies by grouping column
ave_accuracies <- aggregate(feat_imp$accuracy, by=list(feat_imp[,opt$grouping_column,]), mean)
# remove features below threshold
ave_accuracies <- ave_accuracies[ave_accuracies$x > accuracy_threshold, ]

#empty variables to fill in
link_source <- character(0)
link_target <- character(0)
name_source <- character(0)
name_target <- character(0)

#### Find top x elements for each resp feature, make links ####
for (r in 1:nrow(ave_accuracies)){
  rv <- ave_accuracies$Group.1[r]
  score <- ave_accuracies$x[r]
  df <- feat_imp[feat_imp[,opt$grouping_column,] == rv, 4:ncol(feat_imp)]
  ele_means <- colMeans(df)
  top_ele <- sort(ele_means, decreasing = TRUE)[1:top_X]
  for (ele in 1:top_X){
    el <- top_ele[ele]
    print(el)
    # name_source <- c(name_source, rv)
    name_source <- c(name_source, paste(rv, round(score, 3)))
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
                   units = "Ave_score", fontSize = 14,# nodeWidth = 30, 
                   margin = list(50,100,500,100), sinksRight = TRUE,
                   height = opt$plot_height, width = 600)

p <- htmlwidgets::prependContent(p, htmltools::tags$h1(sub('\\..[^\\.]*$', '', basename(opt$featimp_path))))
# p <- htmlwidgets::appendContent(p, htmltools::tags$p("*, SR that included children/adolescents only; ~, SR that included only adults; all other SR included all age groups."))
# p <- htmlwidgets::appendContent(p, htmltools::tags$p("Higgins KA, Rawal R, Kramer M, Baer DJ, Yerke A, Klurfeld DM. An overview of reviews on the association of low calorie sweetener consumption with body weight and adiposity. Advances in Nutrition (accepted)."))
p

print(paste("save network:", opt$out_name))
saveNetwork(p, file=file.path(opt$out_name), selfcontained = TRUE)

print("Make pdf")
Sys.setenv(OPENSSL_CONF="/dev/null")
# webshot::install_phantomjs(force = TRUE)
webshot(url = opt$out_name,
        file = file.path(paste0(sub('\\..[^\\.]*$', '', opt$out_name),".pdf")))#remove extention and add .pdf ext

print("End R script")
