# Author: Aaron Yerke (aaronyerke@gmail.com)
# Centralized location for data org functions used in multiple scripts

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("nhanesA", quietly = TRUE)) BiocManager::install("nhanesA")
library("nhanesA")

print("Loaded dependencies")

#### Defining functions ####
#convert time to minutes (https://stackoverflow.com/questions/40476796/r-function-to-convert-time-of-day-to-total-minutes)
time_dif <- function(x) difftime(as.POSIXct(x, format = "%H:%M"), as.POSIXct("00:00", format = "%H:%M"), units = "min")

nhanes_names <- function(dl_df, dt_group, nh_tble) {
  #puts human readable names on the NHANES tables
  if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
  if (!requireNamespace("nhanesA", quietly = TRUE)) BiocManager::install("nhanesA")
  library("nhanesA")
  diet_names <- nhanesA::nhanesTableVars(data_group = dt_group,
                                         nh_table = nh_tble,
                                         namesonly = FALSE,
                                         nchar = 160)
  
  my_ord <- match(names(dl_df), diet_names$Variable.Name)
  names(dl_df) <- diet_names$Variable.Description[my_ord]
  return(dl_df)
}

download_org_nhanes <- function(dt_group, nh_tble, translate = TRUE) {
  #downloads and relabels table
  if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
  if (!requireNamespace("nhanesA", quietly = TRUE)) BiocManager::install("nhanesA")
  library("nhanesA")
  print(paste(dt_group, nh_tble))
  dl_tble <- nhanesA::nhanes(nh_tble)
  # print(dl_tble)
  # dl_tble <- dl_tble[,sapply(dl_tble, function(x) !all(is.na(x)))]
  if (translate == TRUE){
    dl_tble <- nhanesA::nhanesTranslate(nh_table = nh_tble,
                                        details = TRUE,
                                        colnames = names(dl_tble),
                                        data = dl_tble)
  }
  
  dl_tble <- nhanes_names(dl_tble, dt_group, nh_tble)
  attr(dl_tble, "names") <- gsub("[[:punct:]]$", "", names(dl_tble))
  attr(dl_tble, "names") <- gsub("[\r\n]", "", names(dl_tble))
  attr(dl_tble, "names") <- gsub("[-]", " ", names(dl_tble))
  return(dl_tble)
}

nhanes_dl_save_if_not_local <- function(f_path, dt_grp, nh_tbl, translate = TRUE) {
  if (!file.exists(f_path)){
    print(paste(f_path, "not found - Downloading!"))
    dl_tabl <- download_org_nhanes(dt_group = dt_grp,
                                   nh_tble = nh_tbl,
                                   translate = translate)
    write.csv(dl_tabl, f_path, row.names = FALSE)
    Sys.sleep(2)
  }else{
    dl_tabl <- read.csv(f_path, header = TRUE, check.names = FALSE)
  }
  return(dl_tabl)
}

convert_dummy <- function(df){
  my_list <- sapply(df, function(x){
    if (is.factor(x) | is.character(x)){
      if(length(unique(x)) > 3){
        return(fastDummies::dummy_cols(x, remove_selected_columns = TRUE,
                                       ignore_na = TRUE,
                                       remove_most_frequent_dummy = TRUE))
      }else{
        return(fastDummies::dummy_cols(x, remove_selected_columns = TRUE,
                                       ignore_na = TRUE, remove_first_dummy = TRUE))
      }
    }#END if (is.factor(x))
    else{
      return(x)
    }
  })
  return(data.frame(my_list, check.names = FALSE))
}

save_features <- function(vect = raw_features_used, vec_names = "name", feature_vect){
  names(feature_vect) <- rep(vec_names, length(feature_vect))
  return(c(raw_features_used, feature_vect))
}
tst_df <- data.frame(idvar = c("a", "a", "b", "b", "b", "c"),
                     item =  c("A", "C", "A", "B", "B",  "C"),
                     count = c(1,2, 1,1,2, 5))
dummy_df <- data.frame(idvar = c("a", "b", "c"),
                       item_A = c(1, 1, 0),
                       item_B = c(0, 3, 0),
                       item_C = c(2, 0, 5))
row.names(dummy_df) <- c("a", "b", "c")

two_column_dummy <- function(df, id_colnm, item_colnm, count_colnm) {
  # Function for using one column as the item (dummy car), and another as a count
  # Arguments:
  # dataframe (df) that has at least an id, item, and count column
  # a string for the column name of ids (id_colnm), 
  # a string for the column name of items (item_colnm), 
  # and a string for the column name of counts (count_colnm)
  print(paste(id_colnm, item_colnm, count_colnm))
  uniq_items <- unique(df[,item_colnm])
  uniq_items <- paste0(item_colnm, "_",uniq_items)
  # print(uniq_items)
  id_var <- unique(df[,id_colnm])
  new_df <- data.frame(matrix(0 ,nrow = length(id_var), ncol = length(uniq_items)))
  names(new_df) <- uniq_items
  row.names(new_df) <- id_var
  # print(id_var)
  for (id in 1:nrow(new_df)) {
    my_id <- as.character(id_var[id])
    # print(my_id)
    my_sub <- df[df[,id_colnm]==my_id,#subset of df argument
                 c(item_colnm,count_colnm,
                   id_colnm)]
    for (rw in row.names(my_sub)){
      # print(rw)
      food <- my_sub[rw, item_colnm]
      # print(food)
      my_cell <- new_df[my_id, paste0(item_colnm, "_", food)]
      my_grams <- my_sub[rw, count_colnm]
      new_df[my_id, paste0(item_colnm, "_", food)] <- my_cell + my_grams
    }
  }
  new_df[,id_colnm] <- row.names(new_df)
  new_df <- new_df[ , order(names(new_df))]
  return(new_df)
}
unit_test1 <- two_column_dummy(df = tst_df,
                               id_colnm = "idvar",
                               item_colnm = "item",
                               count_colnm = "count")
identical(unit_test1, dummy_df)

min_max_trans <- function(df) {
  # for min max transformations
  if (!requireNamespace("caret", quietly = TRUE)) BiocManager::install("caret")
  library("caret")
  
  # normalizing data
  ss <- preProcess(as.data.frame(df), method=c("range"))
  
  df <- predict(ss, as.data.frame(df))
  detach("package:caret", unload=TRUE)
  return(df)
}

#for making all the transformations at once
save_all_transforms <- function(out_dir, prefix, df,
                                id_var = "Respondent sequence number" ) {
  print(paste("Creating transformations of", prefix))
  df <- data.frame(df, check.names = FALSE)
  write.csv(df, file = file.path(out_dir, paste0(prefix,".csv")),
            row.names = FALSE)
  # row.names(df) <- df[,id_var]
  # print(ncol(df))
  # num_only <- df[,which(! names(df) %in% c(id_var))]
  # num_only[is.na(num_only)] <- 0
  # print(ncol(num_only))
  # # print(paste("min/maxing", prefix))
  # # df_mm <- min_max_trans(num_only)
  # # df_mm[,id_var] <- row.names(num_only)
  # # write.csv(df_mm, file = file.path(out_dir, paste0(prefix,"_MM.csv")),
  # #           row.names = FALSE)
  # # Z-score standardization, AKA (df - mean(df)) / sd(df)
  # df_scale <- as.data.frame(scale(num_only, center = TRUE, scale = TRUE))
  # df_scale[,id_var] <- row.names(num_only)
  # write.csv(df_scale, file = file.path(out_dir, paste0(prefix,"_SCALE.csv")),
  #           row.names = FALSE)
  # print(paste("logging", prefix))
  # min_val <- min(num_only[num_only > 0])
  # # print(num_only)
  # num_only <- num_only + min_val/10
  # df_log <- log(num_only)
  # df_log[,id_var] <- row.names(num_only)
  # write.csv(df_log, file = file.path(out_dir, paste0(prefix,"_LOG.csv")),
  #           row.names = FALSE)
  print(paste("Saved 4 transformations of", prefix))
  # return(df_log)
}

cod_var <- c("a","b","b","b","c", "c")
ids <- c(2,3,3,1,2,4)

expected_df <- data.frame(cod_var_a = c(0,1,0,0),
                          cod_var_b = c(1,0,2,0),
                          cod_var_c = c(0,1,0,1),
                          ids = c(1,2,3,4))

fake_df <- data.frame(cod_var, ids)

simple_dummy <- function(df, dummy_var, ids, join_symb) {
  #Function to use in place of fastdummies dummyCols because it wasn't working 
  #for my 2009-2020 dietary dataset.
  df[,dummy_var] <- paste0( "_", df[,dummy_var])
  dummy <- data.frame(model.matrix( ~ cod_var -1,
                                    data = df),
                      check.names = F)
  dummy[,ids] <- df[,ids]
  dummy <- aggregate(dummy, by = list(df[,ids]), FUN = sum)
  dummy[,ids] <- dummy$Group.1#put correct labels back on ids column
  drop_cols <- c(paste0(dummy_var, "_", "NA"), "Group.1")
  dummy <- dummy[,!(names(dummy) %in% drop_cols)]#drop "Group.1" column and NA col
  
  return(dummy)
}

print("Functions loaded!")