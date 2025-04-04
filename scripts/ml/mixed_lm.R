# ---
# title: "Mixed linear model"
# output: "pdf_document"
# author: "Aaron Yerke (aaronyerke@gmail.com)"
# ---
  
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("ggplot2", quietly = TRUE))  BiocManager::install("ggplot2")
library("ggplot2")
if (!requireNamespace("readxl", quietly = TRUE))  BiocManager::install("readxl")
library("readxl")
if (!requireNamespace("data.table", quietly = TRUE))  BiocManager::install("data.table")
library("data.table")
if(!require(nlme)){install.packages("nlme")}
library("nlme")
if(!require("lme4")){install.packages("lme4")}
library("lme4")
if(!require("poolr")){install.packages("poolr")}
library("poolr")

print("Libraries are loaded.")

#### Functions ####

#### Parse command line arguments ####
option_list <- list(
  optparse::make_option(c("-p", "--predictors"), type="character",
                        default = "data/metabolomics/meat-demo-log-filt_all_bat_norm_imput-chem.csv",
                        help="path of predictor features csv"),
  optparse::make_option(c("-p", "--response"), type="character",
                        default = "data/CVD/modified/noMap-cvd_markers.csv",
                        help="path of response variables csv"),
  optparse::make_option(c("-s", "--output_dir"), type="character",
                        default = "no_map_auto_protein", help="dir in /output"),
  optparse::make_option(c("-o", "--out_name"), type="character",
                        default = "mixlm",
                        help="Path of output csv.")
);
opt_parser <- optparse::OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

print("Commandline arguments:")
print(opt)

#### Establish directory layout and other constants ####
output_dir <- file.path("output", opt$output_dir)
dir.create(output_dir)

#### Read data ####
orig_pred_df <- read.csv(file = file.path(opt$predictors),
                    header = TRUE, check.names = FALSE)

orig_resp_df <- read.csv(file = file.path(opt$response),
                    header = TRUE, check.names = FALSE,
                    stringsAsFactors=TRUE)

#### Reorganize and preprocess table ####
# Remove rows that aren't in response
resp_df <- orig_resp_df[!is.na(orig_resp_df$ldl),]
print(paste("Number of samples in original pred_df:", nrow(orig_pred_df)))
print(paste("Number of samples in original resp_df:", nrow(resp_df)))
meta_intrsct <- intersect(row.names(orig_pred_df), row.names(resp_df))
print(paste("Predictor and response number of common samples:", length(meta_intrsct)))

#order both data frames the same
pred_df <- orig_pred_df[meta_intrsct,]
resp_df <- resp_df[meta_intrsct,]
print("Rows in same order:")
identical(row.names(resp_df), row.names(pred_df))

#### Filter metabolites ####
#Remove columns that all but 2 NA
pred_df <- pred_df[,!sapply(pred_df, function(x) length(which(is.na(x))) >= nrow(pred_df)-2)]

#Remove features with more than 80% zeros
# NOTE: Keep NA as NA
na_table <- pred_df
na_table[is.na(na_table)] <- 0
pred_df <- pred_df[,!sapply(na_table, function(x) mean(x == 0) > 0.8)]
print(paste("0 rw/col:", paste(dim(pred_df), collapse = "/")))

# my_cols <- names(initial_table)

# <https://www.r-bloggers.com/2017/12/linear-mixed-effect-models-in-r/>
# <https://mspeekenbrink.github.io/sdam-r-companion/linear-mixed-effects-models.html>
# https://developer.nvidia.com/blog/a-comprehensive-guide-to-interaction-terms-in-linear-regression/

helpers <- pred_df[c("age", "bmi", "sex")]
pred_df <- within(pred_df, rm("PARENT_SAMPLE_NAME", "age", "bmi", "sex"))
metaboliteNames <- c()
resp_df_col <- c()
pvals <- c()
failed_metabolites <-c()
# resp_df <- within(resp_df, rm("TIMEPOINT"))
for (i in 1:ncol(pred_df)){
  metaboliteName <- colnames(pred_df)[i]
  sub_df <- resp_df[c("ldl", "SUBJECT_ID", "SITE")]
  sub_df <- cbind(sub_df, helpers)
  sub_df$ldl <- as.numeric(sub_df$ldl)
  # Notes on mixed linear model
  # in y = mx + b notation:
  # ldl = m (metabo) + fixed effect + random effect
  # random = ~ 1 | SITE/, 
  sub_df$metabo <- unlist(pred_df[,i])
  sub_df <- sub_df[complete.cases(sub_df),]
  # remove subject ID where there is 2 or less
  # more_3 <- table(sub_df$SUBJECT_ID) >=3
  # sub_df <- subset(sub_df, SUBJECT_ID %in% names(more_3))
  tryCatch(expr = {
          myLme <- nlme::lme(metabo ~ ldl + ldl*age + ldl * bmi + ldl * sex,
          random = ~ 1 | SITE/SUBJECT_ID,
          data = sub_df)
          
          #Pull out pvalue for diet_order variable to run through pooled R
          #Look into failed
          metaboliteNames <- c(metaboliteNames, metaboliteName)
          cs <- as.data.frame(summary(myLme)$Table)
          my_pval <- anova(myLme)["ldl", "p-value"]
          pvals <- c(pvals, my_pval)
          # myLme = nlme::gls(metabo ~  DIET_ORDER*TIMEPOINT + DIET*TIMEPOINT,
          # na.action=na.omit, data=sub_df,
          # correlation=nlme::corSymm(form= ~ as.integer(TIMEPOINT) | SUBJECT_ID),
          # weights=nlme::varIdent(form=~1|TIMEPOINT))
          },
          error=function(cond) {
            failed_metabolites <<- c(failed_metabolites, metaboliteName)
            print(paste("an error is thrown on iteration", i, metaboliteName))
            message(cond)
            # Choose a return value in case of error
            # return(NA)
          },
          warning=function(cond) {
            failed_metabolites <<- c(failed_metabolites, metaboliteName)
            print(paste("a warning is thrown on iteration", i, metaboliteName))
            message(cond)

            # Choose a return value in case of warning
            # return(NULL)
          }
        )
}

names(pvals) <- metaboliteNames

model1_mean_pval <- mean(pvals)
adj_pvals <- p.adjust(pvals, method = "BH")
model1_mean_adj_pval <- mean(adj_pvals)
sig_metabo <- which(adj_pvals < 0.05)

print(paste("mean dietorder pval for model1:", model1_mean_pval, "\n", sum(pvals < 0.05), "significant out of", length(pvals),"metabolites"))

print(paste("mean dietorder adj_pval for model1:", model1_mean_adj_pval, "\n", sum(adj_pvals < 0.05), "significant out of", length(pvals),"metabolites \n", paste(metaboliteNames[sig_metabo], collapse = ", ")))

#### Make a plot of the top 10 metabolites ####
