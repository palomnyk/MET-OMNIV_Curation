#Author: Aaron Yerke, aaronyerke@gmail.com
#Script to post information about participants demographics. It should take a
#csv and make histograms for the ages and BMI, and bar plots for sex. One for 
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
                        default="data/diet/nutrition_data/all_sites-meats_normalize_full_df.csv",
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

my_table$Sex <- as.character(gsub(1, "male", gsub(0, "female", my_table$Sex)))

num_columns <- c("Age", "BMI", "Sex")
x_ax <- c("Years", expression( 'kg/m' ^ 2), "")

y_lim <- c(30, 40, 260)

#### Organizing metadata ####
# Remove rows that don't have corrected site
my_table <- my_table[!is.na(my_table$SITE),]
my_table <- my_table[!is.na(my_table$Sex),]

pdf(file = file.path(opt$output_fpath),
    width=10*length(num_columns), height=7*length(num_columns))

par(mfrow=c(length(num_columns),5), mar=c(4,4,2,1),#bottom, left, top, right
    cex=2)#oma=c(1,1,4,1)
for (ft in 1:length(num_columns)){
  feat <- num_columns[ft]
  cap_feat <- paste(toupper(substr(feat, 1, 1)), substr(feat, 2, nchar(feat)),
                    sep="")
  # par(mfrow=c(1,5), oma=c(4,1,4,1))
  counts <- my_table[,feat]
  no_na <- sum(!is.na(counts))
  supstring <- x_ax[ft]
  my_y_max <- y_lim[ft]
  if (is.numeric(counts)){
    hist(counts, main = paste("All sites:", feat),
         xlab = supstring,
         breaks=num_breaks, ylab = "Frequency",
         ylim=c(0,my_y_max),
         sub = paste("n=", no_na))
  }else{
    # par(mar=c(20,4,4,4))
    count_table <- as.data.frame(table(counts))
    bp <- barplot(count_table$Freq, main = paste("All sites:",feat),
            ylim=c(0,my_y_max),
            las = 3,
            ylab = "Participants",
            names.arg = count_table$counts,
            sub = paste("n=", no_na))
    text(x = bp, y = count_table$Freq + 1, label = count_table$Freq, pos = 3, cex = 0.8)
    # my_string <- paste(my_string,"\n",str(table(counts)))
    # cat(str(data.frame(table(counts))), file = cat_out_name, append = T)
  }
  for (site in unique(my_table$SITE)){
    df <- my_table[my_table$SITE == site,]
    counts <- df[,feat]
    no_na <- sum(!is.na(counts))
    supstring <- x_ax[ft]
    # print(supstring)
    if (is.numeric(counts)){
      hist(counts, main = paste0(site, ": ", feat),
           xlab = supstring,
           breaks=num_breaks,
           ylim=c(0,my_y_max),
           sub = paste("n=", no_na))
    }else{
      # par(mar=c(20,4,4,4))
      count_table <- as.data.frame(table(counts))
      bp <- barplot(count_table$Freq, main = paste0(site, ": ", feat),
                    ylim=c(0,my_y_max),
                    las = 3,
                    ylab = "Participants",
                    names.arg = count_table$counts,
                    sub = paste("n=", no_na))
      text(x = bp, y = count_table$Freq + 1, label = count_table$Freq, pos = 3, cex = 0.8)
      # my_string <- paste(my_string,"\n",str(table(counts)))
    }
  }
  # Labels for each feature
  mtext(cap_feat, side=3.1, line = 2.95, adj=-2.5, cex=3.5)#side=3 puts it at top
}

dev.off()

print("End of R script!")
