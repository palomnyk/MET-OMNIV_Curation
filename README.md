# beef_biomarkers
This repository was created to document how we organized the dietary data for our beef biomarkers study.
## Harmonizing data
We will have 3 ESHA dietary datasets and one from NDSA. Currently, 2 ESHA datasets have been processed and we are waiting for 1 ESHA and one NSDA.
### harmonization/p1_build_ESHA_csv.R
- Script for building tables from ESHA output - making columns from indention
- Meant to give a starting point to our harmonization dictionaries
- Combines ESHA from multiple projects into the same table
    - currently combines the MAP and MED studies
- Has unit test: harmonization/unit_tests/test_build_ESHA_csv.R
### harmonization/p2_add_FPED_data.R
- Script for adding HEI/FPED categories to ESHA output
- Creates an HEI conversion table and adds HEI variables to the combined ESHA file.
- Has unit test: harmonization/unit_tests/test_add_FPED_data.R
### harmonization/p3_automatic_label_proteins.R
#### Script for labeling the protein sources based off of rudimentary text mining and adding HEI values
##### rudimentary text mining
- Looks for key words in Item_name based off "dictoraries"
- Identifies protein sources and whether it is processed or not
- In script unit testing based on Lauren's hand processed script
##### Used the labeled proteins and the HEI conversion table to add HEI equivalants to combined ESHA reports
### harmonization/p4_impute_missing_from_SR.R
#### Script for imputing missing nutrient data from best match in USDA's SR database
- SR_food.csv comes from the zip file loaded from https://fdc.nal.usda.gov/download-datasets.html
  - food.csv in FoodData_Central_sr_legacy_food_csv_2018-04.zip
  - accessed on 24 Feb 2024



conda env update --file workflow/env/python_conda_env.yml --prune



snakemake --dag |dot -Tpdf > workflow/reports/dag.pdf

snakemake --rulegraph |dot -Tpdf > workflow/reports/rulegraph.pdf

snakemake --forceall --dag |dot -Tpdf > workflow/reports/forceall.pdf
--forceall 
 snakemake --forceall --dag | dot -Tpdf > dag.pdf

snakemake --filegraph --dag | dot -Tpdf > dag.pdf

  -e pred_table, --pred_path pred_table
                        path, relative to cwd, to table with predictor/explanatory/independant vars
  -o OUTPUT_LABEL, --output_label OUTPUT_LABEL
                        base label for output files (additional info will be added to it)
  -c RESPONSE_COL, --response_col RESPONSE_COL
                        Feature column to use in response var, if empty, all will be used
  -f out_sub, --out_folder out_sub
                        path, sub folder in 'output'.
  -r resp_fn, --response_fn resp_fn
                        Name of file that holds response vars.
  -l delim, --delimiter delim
                        File delimiting symbol for metadata. Default is tab.
  -t title, --title title
                        Title for visualizations
  -v id_var, --id_var id_var
                        String, column name of row variables

python scripts/ml/random_forest.py\
  --pred_path data/metabolomics/log_filt_all_bat_norm_imput.csv \
  --response_col beef_level \
  --output_label beef_lev_NO_MAP \
  --out_folder no_med \
  --response_fn data/mapping/noMap_metadata.csv \
  --delimiter , \
  --title "Beef Level Predictions" \
  --id_var "PARENT_SAMPLE_NAME"

python scripts/ml/random_forest.py\
  --pred_path data/metabolomics/log_filt_all_bat_norm_imput.csv \
  --response_col beef_level \
  --output_label beef_lev_NO_MAP \
  --out_folder no_map \
  --response_fn data/mapping/noMap_metadata.csv \
  --delimiter , \
  --title "Beef Level Predictions (3 levels)" \
  --id_var "PARENT_SAMPLE_NAME"

python scripts/ml/random_forest.py\
  --pred_path data/metabolomics/log_filt_all_bat_norm_imput.csv \
  --response_col "TIMEPOINT" \
  --output_label pre_post_treat_NO_MAP \
  --out_folder no_map \
  --response_fn data/mapping/noMap_metadata.csv \
  --delimiter , \
  --title "Pre vs post treatment" \
  --id_var "PARENT_SAMPLE_NAME"


python scripts/ml/random_forest.py\
  --pred_path data/metabolomics/log_filt_all_bat_norm_imput.csv \
  --response_col "beef_level_oz" \
  --output_label beef_level_oz_NO_MAP \
  --out_folder no_map \
  --response_fn data/mapping/rf_noMed_metadata.csv \
  --delimiter , \
  --id_var "PARENT_SAMPLE_NAME"

