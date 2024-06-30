#!/usr/bin/env python
# Author: Aaron Yerke, aaronyerke@gmail.com
# Script for imputing missing nutrition data from SR data
# --------------------------------------------------------------------------
print("Loading external libraries.",flush = True)
# --------------------------------------------------------------------------
import time
import json
import requests
import os
import pandas as pd
# pip install -U sentence-transformers
from sentence_transformers import SentenceTransformer, util

# --------------------------------------------------------------------------
print("Defining functions", flush = True)
# --------------------------------------------------------------------------
    

# --------------------------------------------------------------------------
print("Establishing directory layout.", flush = True)
# --------------------------------------------------------------------------
output_dir = os.path.join(".", "nutrition_data")
if not os.path.exists(output_dir):
	os.makedirs(os.path.join(".",output_dir))
assert os.path.exists(output_dir)

data_dir = os.path.join(".", "nutrition_data")
result_fpath = os.path.join(output_dir, "best_matches", "hf_SR_best_matches.xlsx")
start_flag = 0

# --------------------------------------------------------------------------
print("Establishing other constants.")
# --------------------------------------------------------------------------


dietary_data = pd.read_excel(os.path.join(data_dir, "HEI_with_proportions_long.xlsx"))
#dietary_data contains the HEI and meat labels for each food in esha_studies_leo

sr_table = pd.read_csv(os.path.join(data_dir, "SR", "SR_food.csv"))
#SR table from which we will try to fill in missing values for esha_studies_leo

# Empty variables to fill
query_name = []
best_sr = []
best_score = []
num_same_score = []

model = SentenceTransformer('sentence-transformers/all-MiniLM-L6-v2')

sr_upper = [model.encode(b, convert_to_tensor=True) for b in sr_table["description"]]

# with open(result_fpath, "w+") as fl:
#     fl.write("query,best_SR,best_score\n")
for f_item in range(start_flag,dietary_data.shape[0]):
    print(f"Iteration {f_item}")
    query = model.encode(dietary_data["item"][f_item], convert_to_tensor=True)
    data = [util.pytorch_cos_sim(query, b) for b in sr_upper]
    max_val = max(data)
    max_ind = data.index(max_val)
    best_score.append(max_val)
    best_match = sr_table["description"][max_ind]
    best_sr.append(best_match)
    query_name.append(dietary_data["item"][f_item])

result_df = pd.DataFrame({"query": query_name,
                            "best_sr": best_sr,
                            "best_score": best_score})
result_df.to_excel(result_fpath, index=False)

print("File saved!")
# --------------------------------------------------------------------------
print(f"{__file__} complete!")
# --------------------------------------------------------------------------
