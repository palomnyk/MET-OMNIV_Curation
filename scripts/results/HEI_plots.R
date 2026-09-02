# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for making radar plots of MAP/MED data HEI scores
# Need order of components to be:
# Total Fruits, Whole Fruits, Total Vegetables, Greens & Beans, Whole Grains,
# Dairy, Total Protein Foods, Seafood and Plant Proteins, Fatty Acids,
# Refined Grains, Sodium, Added Sugars, and Saturated Fats. https://epi.grants.cancer.gov/hei/interpret-visualize-hei-scores.html

rm(list = ls()) #clear workspace

print(paste("Working in", getwd()))

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!require("ggplot2")) BiocManager::install("ggplot2")
library("ggplot2")
if (!requireNamespace("readxl", quietly = TRUE))  BiocManager::install("readxl")
library("readxl")
if (!requireNamespace("paletteer", quietly = TRUE))  BiocManager::install("paletteer")
library("paletteer")



print("Loaded dependencies")
source(file.path("scripts","data_org", "data_org_func.R"))

#### Functions ####

#### Establish directory layout and other constants ####
output_dir <- file.path("output", "HEI")
dir.create(file.path(output_dir))
dir.create(file.path(output_dir, "graphics"))
dir.create(file.path(output_dir, "tables"))

#### Loading in data ####
HEI_scores <- read.csv(file = file.path("output", "HEI", "tables", "MAP_MED_HEI_scores_dail_en_const.csv"),
                       header = TRUE, check.names = FALSE)

#### Data reorganization ####
HEI_scores$HEI_category <- factor( HEI_scores$HEI_category, levels = c("Total Fruits","Whole Fruit","Total Vegetable","Greens and Beans",
             "Whole Grains", "Dairy","Total Protein","Seafood and Plant Protein",
             "Fatty Acids","Refined Grain","Sodium","Added Sugar","Saturated Fats",
             "Final Score"))

HEI_scores <- HEI_scores[HEI_scores$Study == "map", ]
HEI_scores$`Component %` <- HEI_scores$`Cutoff points`/HEI_scores$`Max Points` * 100

g <- ggplot(data=HEI_scores,aes(x = HEI_category, y = `Component %`,fill =HEI_category)) +
  # geom_point() +
  facet_grid(~HEI_scores$Intervention) +
  geom_col(position = "dodge") +
  # Labels
  # geom_text(aes(label = round(`Component %`, 1)), vjust = -1, size = 4) +
  # Theme adjustments
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  # theme_minimal() +
  paletteer::scale_fill_paletteer_d("ggthemes::Classic_20") +
  # scale_color_brewer(palette = "Set3") +
  # scale_fill_hue(c = 40, h = c(10,300)) +
  # scale_color_manual(values=c(brewer.pal(12,"Set3"),"#999999"), levels(HEI_scores$HEI_category)) +
  labs(title = "Intervention HEI Scores",
       x = "HEI Category",
       y = "Score as % of possible points")
  ggplot2::ggsave(filename = file.path("output", "HEI", "graphics", "MAP_MED_HEI_scores_dail_en_const.png"),
                  device = "png", width = 20, height = 10)
print(g)



# g <- ggplot(HEI_scores,aes(x = HEI_category, y = `Component %`, group = Intervention, color = Intervention)) +
#   # Stick
#   geom_segment(aes(x = HEI_category, xend = HEI_category, y = 0, yend = `Component %`),
#                linewidth = 1.2) +
#   # Lollipop head
#   geom_point(size = 4) +
#   # Labels
#   geom_text(aes(label = `Component %`), vjust = -1, size = 4) +
#   # Theme adjustments
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
#   theme_minimal() +
#   labs(title = "Lollipop Chart Example",
#        x = "HEI Category",
#        y = "Value")
# print(g)

# for (interv in unique(HEI_scores$Intervention)) {
#   sub_HEI <- HEI_scores[HEI_scores$Intervention == interv,]
#   sub_HEI <- sub_HEI[order(sub_HEI$HEI_category),]
#   # fin_scr <- sub_HEI[sub_HEI$HEI_category == "Final Score", "Cutoff points"]
#   # sub_HEI <- sub_HEI[sub_HEI$HEI_category != "Final Score",]
#   g <- ggplot(sub_HEI,aes(x = HEI_category, y = `Component %`, color = HEI_category)) +
#     # Stick
#     geom_segment(aes(x = HEI_category, xend = HEI_category, y = 0, yend = `Component %`),
#                  linewidth = 1.2) +
#     # Lollipop head
#     geom_point(size = 4) +
#     # Labels
#     geom_text(aes(label = `Component %`), vjust = -1, size = 4) +
#     # Theme adjustments
#     theme_minimal() +
#     labs(title = "Lollipop Chart Example",
#          x = "HEI Category",
#          y = "Value") +
#     ylim(0, 6)
#     print(g)
#   #   geom_polygon(fill = NA, colour = "blue") +
#   #   # geom_line() +
#   #   # geom_path() +
#   #   geom_point(colour = "blue") +
#   #   theme_light() +
#   #   theme(panel.grid.minor = element_blank()) +
#   #   coord_polar(clip = "off", start = 0) +
#   #   labs(x = "", y = "")
#
# }
