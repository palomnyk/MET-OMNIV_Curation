#!/usr/bin/env python
# Author: Aaron Yerke, aaronyerke@gmail.com
# This is a script for random forest for NHANES study
# Should take a table with predictor/explanatory/independant and a table
# with response/outcome/dependant variable.
# Returns accuracy and other metrics and feature importance.
# Interesting resources for SHAP values:
# 	1: https://shap.readthedocs.io/en/latest/example_notebooks/tabular_examples/tree_based_models/NHANES%20I%20Survival%20Model.html
# 		-Uses NHANES data for example
# 	2: https://www.kaggle.com/code/wti200/analysing-interactions-with-shap/notebook

# Note on shap value shape:
# Shape of XGB SHAP values: (143, 30)
# Shape of RF SHAP values: (2, 143, 30)
# Interpretaion:
# XGBoost (143,30) dimensions:
# 143: number of samples in test
# 30: number of features
# RF (2,143,30) dimensions:
# 2: number of output classes
# 143: number of samples
# 30: number of features

# TODO:
# Add options to select shap plots
# Look into:
# ModelStudio (R)
# PDPbox (Python)

# --------------------------------------------------------------------------
print("Loading external libraries.",flush = True)
# --------------------------------------------------------------------------
from cProfile import label
import os, sys
import numpy as np
import pandas as pd
from pandas.api.types import is_numeric_dtype
from sklearn.metrics import accuracy_score, roc_auc_score, r2_score
from sklearn.metrics import confusion_matrix
from sklearn.metrics import ConfusionMatrixDisplay
import matplotlib.pyplot as plt
from matplotlib import rcParams
rcParams.update({'figure.autolayout': True})
import matplotlib.backends.backend_pdf
# matplotlib.use('TKAgg')
from sklearn import model_selection
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import RandomForestRegressor
import argparse
import random
import pathlib
import pdb
import shap
from sklearn import tree
from joblib import dump,load
# import plotnine

# QT_DEBUG_PLUGINS=1
# --------------------------------------------------------------------------
print("Reading commmandline input with optparse.", flush = True)
# --------------------------------------------------------------------------
parser = argparse.ArgumentParser(description="This script runs a random forest test on various datasets.")
parser.add_argument("-e", "--pred_path", dest="pred_table", 
				  default = "data/unit_test/mtc_predictor.csv",
                  help="path, relative to cwd, to table with predictor/explanatory/independant vars",
				  metavar="pred_table")
parser.add_argument("-o", "--output_label", default="py_rf",
				  dest="output_label",
                  help="base label for output files (additional info will be added to it)")
parser.add_argument("-c", "--response_col", default=False,
                  help="Feature column to use in response var, if empty, all will be used")
parser.add_argument("-f", "--out_folder", default="unit_test",
                  help="path, sub folder in 'output'.", 
				  metavar="out_sub")
parser.add_argument("-r", "--response_fn", default="data/unit_test/mtc_response.csv",
				  dest="resp_fn",
                  help="Name of file that holds response vars.", metavar="resp_fn")
parser.add_argument("-l", "--delimiter", default="\t",
                  help="File delimiting symbol for metadata. Default is tab.",
				  metavar="delim", dest="delim")
parser.add_argument("-t", "--title", default=False,
                  help="Title for visualizations",
                  metavar="title", dest="title")
parser.add_argument("-v", "--id_var", default="Respondent sequence number",
                  help="String, column name of row variables",
                  metavar="id_var", dest="id_var")
parser.add_argument("-s", "--respons_df_start", default=0, type=int,
                  help="integer, use if wanting to skip first _____ column of response_fn. use if response_col isn't selected",
                  metavar="respons_df_start", dest="respons_df_start")
parser.add_argument("-m", "--use_saved_model", default=False, type=bool,
                  help="Boolean: use saved model and only make figures, if it exists, or train new model (false).")
parser.add_argument("-n", "--num_cv_folds", default=10, type=int,
                  help="number of folds for crossvalidation.")
parser.add_argument("-T", "--plot_trees", default=False, type=bool,
                  help="Show 5 random trees from RF, yes or no. Will only plot them if score is above ave score threshold")
options, unknown = parser.parse_known_args()

print(options)

# --------------------------------------------------------------------------
print("Defining functions", flush = True)
# --------------------------------------------------------------------------

# --------------------------------------------------------------------------
print("Establishing directory layout.", flush = True)
# --------------------------------------------------------------------------
output_dir = os.path.join(".", "output", options.out_folder)

pathlib.Path(os.path.join(".",output_dir, "graphics")).mkdir(parents=True, exist_ok=True) 
pathlib.Path(os.path.join(".",output_dir, "tables")).mkdir(parents=True, exist_ok=True) 
assert os.path.exists(output_dir)

# --------------------------------------------------------------------------
print("Establishing other constants.")
# --------------------------------------------------------------------------
output_label = options.output_label
col_names = ["model", "response_var"]
num_cv_folds = options.num_cv_folds
n_trees = 1000
bar_shown = 25
shap_shown = 10
dep_fig_size = (12, 9)#fig size for dependency plots
score_threshold_SHAP = 0.01 #score to make SHAP plots after RF runs
col_names = col_names + [f"split{x}" for x in range(num_cv_folds)]
if options.title == False:
	options.title == options.output_label

response_df = pd.read_csv(os.path.join(".",options.resp_fn), \
		sep=",", header=0).replace("TRUE", True).replace("FALSE", False)

#set labels for output files and select column to use in response var
if options.response_col == False:
	response_cols = response_df.columns[options.respons_df_start:]
	resp_col_label = ""
else:
	response_cols = [options.response_col]
	resp_col_label = f"1col{options.response_col}!"
response_df = response_df.sort_values(by = options.id_var)
# print(options.id_var)
pred_df = pd.read_csv(os.path.join(".",options.pred_table), \
		sep=",", header=0, index_col=options.id_var).fillna(0)
print(f"predictor shap: {pred_df.shape}")
pred_df = pred_df.sort_values(by = options.id_var)
id_list = response_df.loc[:,options.id_var]

#output files
output_label = f"{resp_col_label}{options.output_label}".replace("/", "／")
model_storage = pathlib.Path("data","models")
shap_storage = pathlib.Path("data","models")
result_fpath = os.path.join(output_dir, "tables", f"{output_label}_scores.csv")
pdf_fpath = os.path.join(output_dir, "graphics", f"{output_label}_feat_import.pdf")
feat_imp_fpath = os.path.join(output_dir, "tables", f"feat_imp_{output_label}.csv")
shap_imp_fpath = os.path.join(output_dir, "tables", f"shap_feat_imp_{output_label}.csv")

seed = 7

# --------------------------------------------------------------------------
print(f"Building accuracy scores. Results found at {result_fpath}.")
# --------------------------------------------------------------------------
pdf = matplotlib.backends.backend_pdf.PdfPages(pdf_fpath)
with open(result_fpath, "w+") as fl:
	fl.write(",".join(col_names))
	fl.write("\n")
	print(f"There are {len(response_cols)} response columns")
	feature_importance = {}#dict to hold all feat import; keys = features, values = [importance scores]
	feature_resp_var = []#response vars in same order of lists holding the importance scores of feature_importances
	model_type = []#model in same order of lists holding the importance scores of feature_importances
	shap_importance = {}#dict to hold all feat import; keys = features, values = [importance scores]
	shap_resp_var = []#response vars in same order of lists holding the importance scores of feature_importances
	shap_model_type = []#model in same order of lists holding the importance scores of feature_importances
	full_accuracy = []
	shap_accuracy = []
	for resp_var in response_cols:
		pred_path_name = pathlib.PurePath(options.pred_table).name
		clean_resp_var = resp_var.replace("/", ".")
		model_fname = f"sklearnRF-{num_cv_folds}-{n_trees}-{pred_path_name}-{clean_resp_var}.mdl"
		model_path = pathlib.Path(model_storage, model_fname)
		resp_safe_ids = response_df.loc[response_df[resp_var ].notna(), options.id_var]
		intersect_safe_ids = list(set(resp_safe_ids) & set(pred_df.index))
		print(f"num safe ids {len(resp_safe_ids)}")
		if options.use_saved_model == False:
			single_resp_accu = []
			responses = []
			predictions = []
			kfold = model_selection.KFold(n_splits=num_cv_folds, random_state=seed, shuffle=True)
			for train, test in kfold.split(intersect_safe_ids):
				# print(f"{resp_var}: train: {len(train)}, test: {len(test)}", flush = True)
				train = [intersect_safe_ids[x] for x in train]
				test = [intersect_safe_ids[x] for x in test]
				pred_train = pred_df.loc[pred_df.index.isin(train),:]#selects whole dataframe
				pred_train = pred_train.round(decimals=4)
				pred_test = pred_df.loc[pred_df.index.isin(test),:]
				# print(pred_train.shape)
				resp_train = response_df.loc[response_df[options.id_var].isin(train).tolist(), resp_var].convert_dtypes()
				resp_test = response_df.loc[response_df[options.id_var].isin(test).tolist(), resp_var].convert_dtypes()
				# print(f"len resp_train {len(resp_train)}, {resp_train.dtype}")
				if is_numeric_dtype(resp_train) and resp_train.dtype.name != "boolean":
					print(f"going to RandomForestRegressor(), {resp_var }")
					clf = RandomForestRegressor(n_estimators=n_trees)
					model_name = "RF_Regressor"
				else:
					print("going to RandomForestClassifier()")
					clf = RandomForestClassifier(n_estimators=n_trees)
					model_name = "RF_Classifier"
				
				model_type.append(model_name)
				clf.fit(pred_train, resp_train)
				# print(f"len(feat_vales) {len(clf.feature_importances_)}, len(names) {len(clf.feature_names_in_)}")
				#add feature importance to global feature_importance
				k_feat_import = dict(zip(clf.feature_names_in_, clf.feature_importances_))
				for key in k_feat_import:
					# Append the value associated with the 'key' to the list for that 'key' in 'result'.
					if key in feature_importance:
						feature_importance[key].append(k_feat_import[key])
					else:
						feature_importance[key] = [k_feat_import[key]]
				feature_resp_var.append(resp_var)
				modl_predict = clf.predict(pred_test)
				predictions.extend(modl_predict)
				responses.extend(resp_test)
				my_score = clf.score(pred_test, resp_test, sample_weight=None)
				# my_score = r2_score(resp_test, modl_predict, sample_weight=None)
				single_resp_accu.append(my_score)

			final_acc = ",".join(map(str, single_resp_accu))
			msg = f"{model_name},{resp_var },{final_acc}\n"
			print(msg, flush=True)
			fl.write(msg)
			full_accuracy.extend(single_resp_accu)

			print("Saving progress so far.")
			dump(clf, model_path)
			feature_df = pd.DataFrame(feature_importance)
			feature_df.insert(loc = 0,column = "response_var", value = feature_resp_var, allow_duplicates=False)
			feature_df.insert(loc = 1,column = "model_type",value = model_type, allow_duplicates=False)
			feature_df.insert(loc = 2,column = "accuracy",value = full_accuracy, allow_duplicates=False)
			feature_df.to_csv(feat_imp_fpath, index = False)
			pred_train.to_csv(f"{feat_imp_fpath}_pred.csv", index = True)
			# resp_test.to_csv(f"{feat_imp_fpath}_resp.csv", index = False)
		else:
			if model_path.is_file():
				# rebuild feature importance df and select only rows from our response variable
				clf = load(model_path)
				feature_df = pd.read_csv(feat_imp_fpath, sep=",", header=0)
				pred_train = pd.read_csv(f"{feat_imp_fpath}_pred.csv", header=0, index_col=0)
			else:
				print(f"{model_path} does not exist.")
		feature_df = feature_df.loc[feature_df["response_var"] == resp_var,:]
		ave_score = np.mean(feature_df["accuracy"])
		ave_std = np.std(feature_df["accuracy"].tolist(), ddof=1)
		print(f"{resp_var} ave_score = {ave_score}, len ave_score {len(feature_df['accuracy'])}")
		print(f"feature_df['accuracy'].tolist()")
		print(feature_df["accuracy"])
		model_name = list(feature_df["model_type"])[0]
		feature_df = feature_df.iloc[:,3:]
		# Order the features by importance
		# feature_df = feature_df.reindex(feature_df.mean().sort_values(ascending=False).index, axis=1)
		feature_mean = pd.DataFrame(feature_df.mean(axis=0)).sort_values(0, ascending=False)
		feature_std = feature_df.std(axis=0)
		feature_mean.insert(1, "std", feature_std)
		plt.barh(y=feature_mean.index[0:bar_shown], width=feature_mean.iloc[0:bar_shown,0], xerr=feature_mean.iloc[0:bar_shown,1])
		plt.xlabel(f"Top {bar_shown} Relative Importances")
		plt.xticks(rotation="vertical")
		plt.title(f"{options.output_label}\n{model_name} score: {round(ave_score, 3)}, std: {round(ave_std, 3)}")
		plt.suptitle(f"Feature importance: {resp_var}")
		pdf.savefig(bbox_inches='tight')
		plt.close()
		
		if ave_score > score_threshold_SHAP:
			#Plot 5 random trees
			if options.plot_trees:
				fig, axes = plt.subplots(nrows = 1,ncols = 5,figsize = (10,2), dpi=900)
				for index in range(0, 5):
					tree.plot_tree(clf.estimators_[index],
					feature_names = pred_train.columns.tolist(), 
					# class_names=cn,
					filled = True,
					ax = axes[index])
					axes[index].set_title('Estimator: ' + str(index), fontsize = 11)
				# fig.savefig('rf_5trees.png')
				pdf.savefig(bbox_inches='tight')
				plt.close()

		print("Created feature importance figure", flush=True)
		# if not is_numeric_dtype(resp_train) or resp_train.dtype.name == "boolean":
		if model_name == "RF_Classifier" and options.use_saved_model == False:
			print("making confusion matrix", flush=True)
			cnf_matrix = confusion_matrix(responses, predictions)
			disp = ConfusionMatrixDisplay(confusion_matrix=cnf_matrix).plot()
			plt.title(f"{options.title}, {resp_var}")
			pdf.savefig()
			plt.close()
		# explainer = shap.Explainer(clf, pred_train, algorithm="tree", seed=7)
		shap_fname = f"shap_sklearnRF-{num_cv_folds}-{n_trees}-{pred_path_name}-{clean_resp_var}.mdl"
		shap_path = pathlib.Path(model_storage, shap_fname)
		explainer = shap.TreeExplainer(clf)
		# if shap_path.is_file():
		# 	print("using saved shap_values")
		# 	# rebuild feature importance df and select only rows from our response variable
		# 	shap_values = load(shap_path)
		# else:
		# print(f"{model_path} does not exist.")
		print("Creating shap_values", flush=True)
		shap_values = explainer(pred_train, check_additivity=False)
		print("shap_values.shape", flush=True)
		print(shap_values.shape, flush=True)

		top_features = feature_mean.index[0:4].tolist()
		if model_name == "RF_Classifier":
			imp_use = shap_values.values[:,1]
			shap_use = shap_values[:,:,1]
			wf_use = shap_values[0,0]
			dep_use = explainer.shap_values(pred_train, check_additivity=False)[:,:,1]
			# shap.plots.beeswarm(shap_values[:,:,1], max_display=shap_shown, show = False)
		else:
			shap_use = shap_values
			wf_use = shap_values[0]
			dep_use = explainer.shap_values(pred_train)
			# record shap feature importance
			vals = np.abs(shap_values.values).mean(0)
			print("vals")
			print(vals)
			print(pred_train.columns)
			shap_dict = dict(zip(pred_train.columns, vals))
			print(shap_dict)
			for key in shap_dict:
				# Append the value associated with the 'key' to the list for that 'key' in 'result'.
				if key in shap_importance:
					shap_importance[key].append(shap_dict[key])
				else:
					shap_importance[key] = [shap_dict[key]]
			shap_resp_var.append(resp_var)
			shap_model_type.append(model_name)
			shap_accuracy.append(np.mean(single_resp_accu))

		try:
			print(f"trying beeswarm plot {model_name} ", flush=True)
			shap.plots.beeswarm(shap_use, max_display=shap_shown, show = False)
			plt.suptitle(f"Final cross val, {resp_var}, {model_name} score: {round(ave_score, 3)}")
			pdf.savefig(bbox_inches='tight')
			plt.close()

			shap.plots.waterfall(wf_use, max_display=shap_shown, show=False)
			#works like this: shap.plots.waterfall(explanation[id_to_explain,:,output_to_explain])
			pdf.savefig(bbox_inches='tight')
			plt.close()

			fig, axes = plt.subplots(nrows=2, ncols=2, figsize=(10, 6))
			axes = axes.ravel() #flattens a n dimentional object to 1 d				
			for i, dep in enumerate(top_features):
				print(f"{i} {dep}, {model_name}", flush=True)
				shap.dependence_plot(dep, dep_use, pred_train, show = False, ax=axes[i])
				plt.suptitle(f"Final cross validation\nSHAP dependency, {resp_var}")
			pdf.savefig(bbox_inches='tight')
			plt.close()
			# matplotlib.rcParams.update(matplotlib.rcParamsDefault)#restore to default
		except Exception as e:
			print(f"Exception: shap beeswarm {model_name} {resp_var}", flush=True)
			print(e, flush=True)	
		try:
			print("TreeExplainer", flush=True)
			# tree_pdf_fpath = os.path.join(output_dir, "graphics", f"{output_label}_{resp_var}", f"{output_label}_{resp_var}_SHAPtree.pdf")
			# tree_pdf = matplotlib.backends.backend_pdf.PdfPages(tree_pdf_fpath)
			shap_values = explainer.shap_values(pred_train)
			print("shap_values.shape", flush=True)
			shap.decision_plot(explainer.expected_value, dep_use, pred_train, show=False)
			plt.suptitle(f"Final cross validation\nSHAP decision, {resp_var}")
			pdf.savefig()
			plt.close()
		except Exception as e:
			print(f"Exception: shap decision {resp_var}", flush=True)
			print(e, flush=True)			
		print(f"Completed SHAP figures", flush=True)
	print("Saving SHAP feature importance", flush=True)
	shap_save = pd.DataFrame(shap_importance)
	print(shap_save)
	print(shap_model_type)
	shap_save.insert(loc = 0,column = "response_var", value = shap_resp_var, allow_duplicates=False)
	shap_save.insert(loc = 1,column = "model_type",value = shap_model_type, allow_duplicates=False)
	shap_save.insert(loc = 2,column = "accuracy",value = shap_accuracy, allow_duplicates=False)
	shap_save.to_csv(shap_imp_fpath, index = False)

print("Saving pdf", flush = True)
pdf.close()

print(f"Completed {__file__}", flush = True)

