#!/usr/bin/env python
# Author: Aaron Yerke, aaronyerke@gmail.com
# This is a script for random forest where training and testing sets are
# specified manually in 4 separate files (training pred, testing pred, 
# training resp, testing pred).
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
parser.add_argument("-a", "--pred_train", dest="pred_train", 
				  default = "data/unit_test/mtc_predictor.csv",
                  help="path, relative to cwd, to train with predictor/explanatory/independant vars",
				  metavar="pred_train")
parser.add_argument("-e", "--pred_test", dest="pred_test", 
				  default = "data/unit_test/mtc_predictor.csv",
                  help="path, relative to cwd, to test with predictor/explanatory/independant vars",
				  metavar="pred_test")
parser.add_argument("-n", "--response_train_fn", default="data/unit_test/mtc_response.csv",
				  dest="resp_train_fn",
                  help="Name of file that holds response train vars.")
parser.add_argument("-r", "--response_test_fn", default="data/unit_test/mtc_response.csv",
				  dest="resp_test_fn",
                  help="Name of file that holds response test vars.",)
parser.add_argument("-o", "--output_label", default="py_rf",
				  dest="output_label",
                  help="base label for output files (additional info will be added to it)")
parser.add_argument("-c", "--response_col", default=False,
                  help="Feature column to use in response var, if empty, all will be used")
parser.add_argument("-f", "--out_folder", default="unit_test",
                  help="path, sub folder in 'output'.", 
				  metavar="out_sub")
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
n_trees = 1000
bar_shown = 25
shap_shown = 10
dep_fig_size = (12, 9)#fig size for dependency plots
score_threshold_SHAP = 0.01 #score to make SHAP plots after RF runs
col_names = col_names + ["split 0"]
if options.title == False:
	options.title == options.output_label

resp_train_df = pd.read_csv(os.path.join(".",options.resp_train_fn), \
		sep=",", header=0).replace("TRUE", True).replace("FALSE", False)

resp_test_df = pd.read_csv(os.path.join(".",options.resp_test_fn), \
		sep=",", header=0).replace("TRUE", True).replace("FALSE", False)

#set labels for output files and select column to use in response var
if options.response_col == False:
	response_cols = resp_train_df.columns[options.respons_df_start:]
	resp_col_label = ""
else:
	response_cols = [options.response_col]
	resp_col_label = f"1col{options.response_col}!"
resp_train_df = resp_train_df.sort_values(by = options.id_var)
resp_test_df = resp_test_df.sort_values(by = options.id_var)
# print(options.id_var)
pred_train_df = pd.read_csv(os.path.join(".",options.pred_train), \
		sep=",", header=0, index_col=options.id_var).fillna(0)
pred_test_df = pd.read_csv(os.path.join(".",options.pred_test), \
		sep=",", header=0, index_col=options.id_var).fillna(0)
print(f"predictor shap: {pred_train_df.shape}")
print(f"predictor shap: {pred_test_df.shape}")
pred_train_df = pred_train_df.sort_values(by = options.id_var)
pred_test_df = pred_test_df.sort_values(by = options.id_var)
# test_id_list = resp_test_df.loc[:,options.id_var]
# train_id_list = resp_train_df.loc[:,options.id_var]
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
		print(f"Resp var: {resp_var}")
		pred_path_name = pathlib.PurePath(options.pred_train).name
		clean_resp_var = resp_var.replace("/", ".")
		model_fname = f"sklearnRF-0-{n_trees}-{pred_path_name}-{clean_resp_var}-train_test_sep.mdl"
		model_path = pathlib.Path(model_storage, model_fname)
		train_resp_safe_ids = resp_train_df.loc[resp_train_df[resp_var].notna(), options.id_var]
		print(f"Length train_resp_safe_ids {len(train_resp_safe_ids)}")
		test_resp_safe_ids = resp_test_df.loc[resp_test_df[resp_var].notna(), options.id_var]
		train_intersect_safe_ids = list(set(train_resp_safe_ids) & set(pred_train_df.index))
		print(f"Length train_intersect_safe_ids {len(train_intersect_safe_ids)}")
		test_intersect_safe_ids = list(set(test_resp_safe_ids) & set(pred_test_df.index))
		print(f"num safe train ids {len(train_resp_safe_ids)}")
		print(f"num safe test ids {len(test_resp_safe_ids)}")
		if options.use_saved_model == False:
			single_resp_accu = []
			responses = []
			predictions = []
			
			pred_train = pred_train_df.loc[pred_train_df.index.isin(train_resp_safe_ids),:]
			pred_train = pred_train.round(decimals=4)
			pred_test = pred_test_df.loc[pred_test_df.index.isin(test_resp_safe_ids),:]
			pred_test = pred_test.round(decimals=4)
			# print(pred_train.shape)
			resp_train = resp_train_df.loc[resp_train_df[options.id_var].isin(train_resp_safe_ids).tolist(), resp_var].convert_dtypes()
			resp_test = resp_test_df.loc[resp_test_df[options.id_var].isin(test_resp_safe_ids).tolist(), resp_var].convert_dtypes()
			# print(f"len resp_train {len(resp_train)}, {resp_train.dtype}")
			if is_numeric_dtype(resp_train) and resp_train.dtype.name != "boolean":
				print(f"going to RandomForestRegressor(), {resp_var }")
				clf = RandomForestRegressor(n_estimators=100, criterion="poisson", max_depth=None, max_features="sqrt", min_samples_leaf=1, min_samples_split=5)
				model_name = "RF_Regressor"
			else:
				print("going to RandomForestClassifier()")
				clf = RandomForestClassifier(n_estimators=n_trees)
				model_name = "RF_Classifier"
			
			model_type.append(model_name)
			print(f"SHAPES pred_train_df: {pred_train_df.shape}, resp_train: {resp_train_df.shape}")
			print(f"SHAPES pred_train: {pred_train.shape}, resp_train: {resp_train.shape}")
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
			print(f"SHAPES pred_test_df: {pred_test_df.shape}, resp_test: {resp_test_df.shape}")
			print(f"SHAPES pred_test: {pred_test.shape}, resp_test: {resp_test.shape}")
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
		plt.title(f"{options.output_label},n\{model_name} score: {round(ave_score, 3)}")
		plt.suptitle(f"Feature importance: {resp_var}")
		pdf.savefig(bbox_inches='tight')
		plt.close()
		
		# if ave_score > score_threshold_SHAP:
		#Plot 5 random trees
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
		shap_fname = f"shap_sklearnRF-no_cv-{n_trees}-{pred_path_name}-{clean_resp_var}.mdl"
		shap_path = pathlib.Path(model_storage, shap_fname)
		explainer = shap.TreeExplainer(clf)
		# if shap_path.is_file():
		# 	print("using saved shap_values")
		# 	# rebuild feature importance df and select only rows from our response variable
		# 	shap_values = load(shap_path)
		# else:
		# print(f"{model_path} does not exist.")
		print("Creating shap_values", flush=True)
		shap_values = explainer(pred_train, check_additivity=False)#[feature_mean.index[0:bar_shown]]
		print("shap_values.shape", flush=True)
		print(shap_values.shape, flush=True)

		top_features = feature_mean.index[0:4].tolist()
		if model_name == "RF_Classifier":
			imp_use = shap_values.values[:,1]
			shap_use = shap_values[:,:,1]
			# print(clf.classes_)
			wf_use = shap_values[0,0]
			dep_use = explainer.shap_values(pred_train)[:,:,1]
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

