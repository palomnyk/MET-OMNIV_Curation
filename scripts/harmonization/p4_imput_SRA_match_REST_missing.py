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

# --------------------------------------------------------------------------
print("Defining functions", flush = True)
# --------------------------------------------------------------------------
def query(payload):
    headers = {"Authorization": f"Bearer {api_token}",
               "wait_for_model": "True"}
    response = requests.post(API_URL,  headers=headers, json=payload)
    return response.json()

# --------------------------------------------------------------------------
print("Establishing directory layout.", flush = True)
# --------------------------------------------------------------------------
output_dir = os.path.join(".", "nutrition_data")
if not os.path.exists(output_dir):
	os.makedirs(os.path.join(".",output_dir, "graphics"))
	os.makedirs(os.path.join(".",output_dir, "tables"))
assert os.path.exists(output_dir)

data_dir = os.path.join(".", "nutrition_data")
result_fpath = os.path.join(output_dir, "best_matches", "hf_SR_best_matches_missing.xlsx")
start_flag = 0

# --------------------------------------------------------------------------
print("Establishing other constants.")
# --------------------------------------------------------------------------
API_URL = "https://api-inference.huggingface.co/models/sentence-transformers/all-MiniLM-L6-v2"
secrets = pd.read_csv("Secrets.csv", index_col=0)
print(secrets)
api_token = secrets.loc["hf_token", "secret"]
dietary_data = pd.read_excel(os.path.join(data_dir, "HEI_with_proportions_long.xlsx"))
#dietary_data contains the HEI and meat labels for each food in esha_studies_leo

sr_table = pd.read_csv(os.path.join(data_dir, "SR", "SR_food.csv"))
#SR table from which we will try to fill in missing values for esha_studies_leo

query_name = []
best_sr = []
best_score = []
num_same_score = []

sr_upper = [b.upper() for b in sr_table["description"]]

# with open(result_fpath, "w+") as fl:
#     fl.write("query,best_SR,best_score\n")
for f_item in range(start_flag,len(dietary_data)):
    print(f"Iteration {f_item}")
    query_desc = dietary_data[f_item]
    time.sleep(0.4)
    try:  
        data = query({
            "inputs": {
                "source_sentence": query_desc,
                "sentences":sr_upper
            }
        })
        print(data)
        print(type(data))
        max_val = max(data)
        max_ind = data.index(max_val)
        best_score.append(max_val)
        best_match = sr_table["description"][max_ind]
        best_sr.append(best_match)
        query_name.append(dietary_data[f_item])
    except:
        print("Something went wrong, not saving")
        # my_range = range(start_flag,f_item-1)
        # print(f"Lens {len(my_range)} {len(best_sr)} {len(best_score)}")
        # new_results = pd.DataFrame({"query": query_name,
        #                            "best_sr": best_sr,
        #                            "best_score": best_score})
        # if os.path.exists(result_fpath):
        #     print("found old result_fpath")
        #     result_df = pd.read_excel(result_fpath)
        #     result_df = pd.concat([result_df, new_results], axis=0)
        #     result_df.to_excel(result_fpath, index=False)
        # else:
        #     new_results.to_excel(result_fpath)
        # print("File saved!")
new_results = pd.DataFrame({"query": query_name,
                            "best_sr": best_sr,
                            "best_score": best_score})
if os.path.exists(result_fpath):
    print("found old result_fpath")
    result_df = pd.read_excel(result_fpath)
    result_df = pd.concat([result_df, new_results], axis=0)
    result_df.drop_duplicates()
    result_df.to_excel(result_fpath, index=False)
else:
    new_results.to_excel(result_fpath)
print("File saved!")
# --------------------------------------------------------------------------
print(f"{__file__} complete!")
# --------------------------------------------------------------------------
