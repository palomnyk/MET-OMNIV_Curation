#Author: Aaron Yerke (aaronyerke@gmail.com)
#Script for imputing missing nutrition data from SR data

# TODO weight first word as highest - Split both at comma's
# Calculate absolute best score and then set threshold for our max score
# Incorporate length and position
# Record top 3 matches so we can check by hand
# Bias towards simplest SR code description
# Incorporate "skip" words - also only use words that are 3 char or more

rm(list = ls()) #clear workspace

#### Loading libraries and data ####
#read in libraries
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("ggplot2", quietly = TRUE))  BiocManager::install("ggplot2")
library("ggplot2")
if (!requireNamespace("openxlsx", quietly = TRUE))  BiocManager::install("openxlsx")
library("openxlsx")
if (!requireNamespace("data.table", quietly = TRUE))  BiocManager::install("data.table")
library("data.table")
print("Libraries are loaded.")

#Read in data
data_dir <- file.path("nutrition_data")

esha_studies_leo <- openxlsx::read.xlsx(xlsxFile = file.path(data_dir, "esha_combined_meats_HEI_vals28Sep2023.xlsx"),
                                     sheet = 1)
#esha_studies_leo combines the micro-nutrient esha output from MAP/MED studies

dietary_data <- openxlsx::read.xlsx(xlsxFile = file.path(data_dir, "HEI_with_proportions_long.xlsx"),
                                           sheet = 1)
#dietary_data contains the HEI and meat labels for each food in esha_studies_leo

sr_table <- openxlsx::read.xlsx(xlsxFile = file.path(data_dir, "SR", "NUT_DATA_Crosstab.xlsx"))
#SR table from which we will try to fill in missing values for esha_studies_leo

sr_header_def <- openxlsx::read.xlsx(xlsxFile = file.path(data_dir, "SR", "NUTR_DEF.xlsx"))
#headers for sr_table

words_to_omit_from_search <- c("with", "and", "a", "to")
#to be used for label processing

SR_abrev_words <- c("BONELESS" = "BNLESS",
                    "DARK" = "DK",
                    "RAW" = "RW",
                    "SLICED" = "SLC",
                    "FROSTED" = "FRSTD",#Must come before ROASTED
                    "ROASTED" = "RSTD",
                    "ROAST" = "RST", #Must come after ROASTED
                    "SWEETENED" = "SWTND",
                    "TOASTED" = "TSTD",
                    "CREAMED" = "CRM",
                    "CANNED" = "CND",
                    "WATER" = "H2O",
                    "ROUND" = "RND",
                    "LEAN" = "LN",
                    "BEEF" = "BF", 
                    "BEANS" = "BNS",
                    "CHUCK" = "CHK",
                    "SHOULDER" = "SHLDR",
                    "BAKED" = "BKD",
                    "WITH" = "W/",
                    "WITHOUT" = "WO/",
                    "SPECIAL" = "SPL",
                    # "APPLE " = "APPL ",
                    "CEREAL" = "CRL",
                    "WHOLE" = "WHL",
                    "SAUCE," = "SAU,",
                    "SAUCE " = "SAU ",
                    "TOMATO" = "TMTO",
                    "GARLIC" = "GRLIC",
                    "ITALIAN," = "ITAL,",
                    "ITALIAN " = "ITAL ",
                    "FROZEN" = "FRZ"
)

#### Add labels to "sr_table" from definitions table ####
sr_header_def$Nutr_No <- as.character(sr_header_def$Nutr_No)#same mode as sr_table

old_cols <- colnames(sr_table)[3:ncol(sr_table)]#Columns to look up

sr_cols <- lapply(old_cols, function(x){
  return(sr_header_def[which(sr_header_def$Nutr_No == x), "NutrDesc"])
})#returns new column names 

sr_cols <- c(colnames(sr_table)[1:2], unlist(sr_cols))

colnames(sr_table) <- sr_cols

openxlsx::write.xlsx(sr_table, file = file.path(data_dir, "SR", "final_sr_table.xlsx"))

#### Find matches between dietary items and sr short descriptions ####
best_matches <- data.frame(#"esha_dscr" = vector(mode = "character", length = nrow(dietary_data)),
                           "sr_dscr_1" = vector(mode = "character", length = 0),
                           "match_score_1" = vector(mode = "integer", length = 0),
                           "num_same_max" = vector(mode = "integer", length = 0),
                           "sr_dscr_2" = vector(mode = "character", length = 0),
                           "match_score_2" = vector(mode = "integer", length = 0),
                           "sr_dscr_3" = vector(mode = "character", length = 0),
                           "match_score_3" = vector(mode = "integer", length = 0))

"
Cycle through each food in the dietary data;
Convert it to upper, remove punctuation;
Split it at punctuation (create words);
If any word is in the SR_abreviations, add the abreviation to the list of words;
Score is zero;

Cycle through each item in SR table;
If any word is there, add square of length of word to score, 
  unless it is first word of either ESHA or SR - then add cube of length;
After adding scores for all words of single food, divide by difference of length of SR item

Sort scores and take top three.

Repeat for ESHA food description.
"
for (itm2 in 1:nrow(dietary_data)) {
  esha_item <- toupper( dietary_data$item[itm2])
  itm_no_punct <- gsub("[();:.]", "", esha_item)
  esha_words <- unique(unlist(strsplit(itm_no_punct, c("[, ]+"))))
  # print(paste(itm_no_punct))
  # print(paste(esha_words, collapse = "::"))
  dietary_scores <- vector(mode = "integer", length = nrow(sr_table))
  names(dietary_scores) <- sr_table$Shrt_Desc
  # best_possible_score <- sum(unlist(lapply(esha_words, function(x) nchar(x)^2)))
  for (itm1 in 1:length(dietary_scores)) {
    sr_desc <- toupper(names(dietary_scores)[itm1])
    sr_desc_no_punct <- gsub("[();:., ]"," ", sr_desc)
    # print(sr_desc_no_punct)
    #some words are abvreviated by SR, adding abbreviations of important words
    for (abrv in 1:length(SR_abrev_words)){
      abrv_word <- SR_abrev_words[abrv]
      sr_desc_no_punct <- gsub(abrv_word, names(abrv_word)[1], sr_desc_no_punct)
    }
    # print("SR")
    # print(sr_desc_no_punct)
    sr_words <- unique(unlist(strsplit(sr_desc_no_punct, ",")))
    # "([.-])|[[:punct:]]"
    score <- 0
    correct_nchar <- 0
    for (w in 1:length(esha_words)){
      word <- esha_words[w]
      prev_match_1st_index <- 0
      prev_match_last_index <- 0
      # print(paste(word, sr_desc_no_punct, sep = "::"))
      grep_query <- grep(word, sr_words,
                          fixed = TRUE,
                         value = FALSE)
      # 
      # print(grep_query)
      if (length(grep_query) > 0){
        # print("grep_query")
        # print(grep_query)
        # print(paste(word, collapse = "//"))
        # print(paste(sr_words, collapse = "::"))
        if (grepl(paste0("^", word), sr_desc_no_punct) && w == 1){#sr and esha start with same word
          score <- score + nchar(word)^2
          correct_nchar <- correct_nchar + nchar(word)
        }else{
          if (grepl(paste0("^", word), sr_desc_no_punct)){
            score <- score + nchar(word)^2
            correct_nchar <- correct_nchar + nchar(word)
          }else{
            score <- score + nchar(word)^2
            correct_nchar <- correct_nchar + nchar(word)
          }# End 2nd else
        }# End 1st else
      }# End if grep
    }# End for word
    sr_nchar_dif <- abs(nchar(sr_desc_no_punct) - nchar(itm_no_punct))
    dietary_scores[itm1] <- score - sr_nchar_dif*1.6
    # print(paste(score, sr_nchar_dif, correct_nchar, abs((sr_nchar_dif - correct_nchar)+1), score/abs((sr_nchar_dif - correct_nchar)+1)))
    # names(dietary_scores)[itm1] <- sr_desc_no_punct
  }# End for itm1
  dietary_scores <- sort(dietary_scores, decreasing = TRUE)
  num_max <- length(which(dietary_scores == dietary_scores[1]))
  best_matches[esha_item,] <- c(names(dietary_scores)[1],
                              dietary_scores[1],
                              num_max,
                              names(dietary_scores)[2],
                              dietary_scores[2],
                              names(dietary_scores)[3],
                              dietary_scores[3])
}# End for itm2

write.csv(best_matches, file = file.path("nutrition_data", "best_matches", "connected words_nchar2.csv"))
