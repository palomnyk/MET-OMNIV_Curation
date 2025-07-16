#!/usr/bin/env python
# Author: Aaron Yerke, aaronyerke@gmail.com
# This is a script for finding the right hyperparameters for the random forest
# regressor for the biomarkers study. It will optomize using the metabolomic data
# ass the predictor/explanatory/independent and beef consumption as the
# response/outcome/dependent variable.
#
# Useful rescourcees for develping this script:
# 	1. https://scikit-learn.org/stable/modules/generated/sklearn.ensemble.RandomForestRegressor.html
# 	2. https://scikit-learn.org/stable/modules/grid_search.html
# 	3. https://scikit-learn.org/stable/modules/generated/sklearn.model_selection.GridSearchCV.html#gallery-examples

# --------------------------------------------------------------------------
print("Loading external libraries.",flush = True)
# --------------------------------------------------------------------------
import os, sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.ensemble import RandomForestRegressor
import pathlib
from sklearn.model_selection import GridSearchCV

# --------------------------------------------------------------------------
print("Establishing directory layout.", flush = True)
# --------------------------------------------------------------------------
output_dir = os.path.join(".", "output", "hyper_param")

pathlib.Path(os.path.join(".",output_dir, "graphics")).mkdir(parents=True, exist_ok=True) 
pathlib.Path(os.path.join(".",output_dir, "tables")).mkdir(parents=True, exist_ok=True) 
assert os.path.exists(output_dir)

# --------------------------------------------------------------------------
print("Establishing other constants.")
# --------------------------------------------------------------------------
id_var = "PARENT_SAMPLE_NAME"
gs_parameters = {
	"n_estimators": (100, 500, 1000, 2000),
	"criterion": ["squared_error", "absolute_error", "friedman_mse", "poisson"],
	"max_features": ("sqrt", "log2"),
	"max_depth": [None, 10, 20],
    "min_samples_split": [1, 2, 5],
    "min_samples_leaf": [1, 2, 10],}

# gs_parameters = {
# 	"n_estimators": (100, 500),
# 	"criterion": ["squared_error", "poisson"],
# 	"max_features": ("sqrt", "log2"),
# 	"max_depth": [None, 10, 20],
#     "min_samples_split": [1, 2, 5],
#     "min_samples_leaf": [1, 2, 10],}
expl_table_path = "data/metabolomics/demo-log-filt_all_bat_norm_imput-chem.csv"
expl_df = pd.read_csv(os.path.join(".", expl_table_path), \
		sep=",", header=0, index_col=id_var).fillna(0)
outcom_table_path = "data/mapping/all_sites-rf_meats_g.csv"
outcom_df = pd.read_csv(os.path.join(".", outcom_table_path), \
		sep=",", header=0).replace("TRUE", True).replace("FALSE", False)
outcom_var = "beef_g"

# --------------------------------------------------------------------------
print("Organizing data.", flush = True)
# --------------------------------------------------------------------------
safe_ids = outcom_df.loc[outcom_df[outcom_var].notna(), id_var]
intersect_safe_ids = list(set(safe_ids) & set(expl_df.index))
expl_df = expl_df.loc[expl_df.index.isin(safe_ids),:]
expl_df = expl_df.round(decimals=4)

outcom_array = outcom_df.loc[outcom_df[id_var].isin(safe_ids).tolist(), outcom_var].convert_dtypes()

print(f"outcome array: {len(outcom_array)}, explain_df: {len(expl_df)}")
# --------------------------------------------------------------------------
print("Create RFR and perform gridsearch.", flush = True)
# --------------------------------------------------------------------------
grid_search = GridSearchCV(RandomForestRegressor(), param_grid=gs_parameters, cv=5)
grid_search.fit(expl_df, outcom_array)

sorted(grid_search.cv_results_.keys())

print("Best Parameters:", grid_search.best_params_)
print("Best Estimator:", grid_search.best_estimator_)