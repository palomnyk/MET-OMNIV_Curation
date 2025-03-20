#Author: Aaron Yerke, aaronyerke@gmail.com
#Script to post information about participants demographics. It should takea a
#csv and make histograms for the ages and bmi, and a pie plot for age. One for 
#each corrected site, plus a combined histogram for the entire study.
#Requires df with all metadata - including demographic data.

rm(list = ls()) #clear workspace

#### Loading dependencies ####
# if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library("ggplot2")
if (!requireNamespace("optparse", quietly = TRUE)) install.packages("optparse")
library("optparse")
# --------------------------------------------------------------------------
print("Reading cml arguments")
# --------------------------------------------------------------------------
option_list <- list(
  optparse::make_option(c("-a", "--data"), type="character",
                        default="data/mapping/noMap_metadata_demo.csv",
                        help="filename in tables folder"),
  optparse::make_option(c("-l", "--delim"), type="character", default="\t",
                        help="metadata file deliminator", metavar="character"),
  optparse::make_option(c("-o", "--output_fpath"), type="character", 
                        default="output/no_map/graphics/demo_plots.pdf",
                        help="relative output file path"),
  optparse::make_option(c("-d", "--output_dir"), type="character", 
                        default="no_map",
                        help="folder in output dir")
); 

opt_parser <- optparse::OptionParser(option_list=option_list);

opt <- optparse::parse_args(opt_parser);

print(opt)

# --------------------------------------------------------------------------
print("Establishing directory layout and other constants.")
#### Establish directory layout and other constants ####
output_label <- opt$output_fname
output_dir <- file.path("output", opt$output_dir)
dir.create(output_dir)
dir.create(file.path(output_dir, "graphics"))
dir.create(file.path(output_dir, "tables"))

num_breaks <- 50

#### Loading in data ####
my_table <- read.csv(file = file.path(opt$data),
                     header = T,
                     check.names =F)

my_table$sex <- as.character(gsub(1, "male", gsub(0, "female", my_table$sex)))

num_columns <- c("age", "bmi", "sex")

#### Organizing metadata ####
# Remove rows that don't have corrected site
my_table <- my_table[!is.na(my_table$SITE),]
my_table <- my_table[!is.na(my_table$sex),]

pdf(file = file.path(opt$output_fpath),
    width=10*length(num_columns), height=7*length(num_columns))

par(mfrow=c(length(num_columns),5), cex.sub=3, mar=c(5,2,5,1),
    cex=2)#oma=c(1,1,4,1)
for (feat in num_columns){
  cap_feat <- paste(toupper(substr(feat, 1, 1)), substr(feat, 2, nchar(feat)), sep="")
  # par(mfrow=c(1,5), cex.sub=1.5, oma=c(1,1,4,1))
  counts <- my_table[,feat]
  no_na <- sum(!is.na(counts))
  supstring <- paste("n:", no_na)
  if (is.numeric(counts)){
    hist(counts, main = paste("all sites"),
         xlab = supstring, breaks=num_breaks)
  }else{
    # par(mar=c(20,4,4,4))
    barplot(table(counts), main = paste("all sites"),
            las = 3,
            xlab = supstring)
    # my_string <- paste(my_string,"\n",str(table(counts)))
    # cat(str(data.frame(table(counts))), file = cat_out_name, append = T)
  }
  for (site in unique(my_table$SITE)){
    df <- my_table[my_table$SITE == site,]
    counts <- df[,feat]
    no_na <- sum(!is.na(counts))
    supstring <- paste("n:", no_na)
    # print(supstring)
    if (is.numeric(counts)){
      hist(counts, main = paste(site),
           xlab = supstring, breaks=num_breaks)
    }else{
      # par(mar=c(20,4,4,4))
      barplot(table(counts), main = paste(site), las = 3,
              xlab = supstring)
      # my_string <- paste(my_string,"\n",str(table(counts)))
      # cat(str(data.frame(table(counts))), file = cat_out_name, append = T)
    }
  }
  mtext(cap_feat, side=3.1, line = 2.95, adj=-2.5, cex=3.5)#side=3 puts it at top
}

# agg_table <- aggregate(my_table$sex, by=list(my_table$CORRECTED_SITE), sum)


# y: number of participants
# x: site
# stacked colors: sex

dev.off()

print("End of R script!")
