# Beef biomarkers and cardiovascular marker studies
This repository was created to document how we organized and analyzed the dietary data for our beef biomarkers and cardiovascular markers studies.

## Overview of workflow
This repository is meant to be run through [Snakemake](https://snakemake.readthedocs.io/en/stable/index.html "Snakemake"), which is a python retooling of the old UNIX tool called "Make" on the USDA SCINET HPC cluster.

The flow of this workflow is dictated through Snakemake rules found in the Snakefile in the workflow folder. These rules follow the following flow:
![rulegraph 1](workflow\reports\rulegraph.png "Flow of rules")

![directed acyclic graph](workflow/reports/dag.png "Rules with wildcards")

### Harmonizing data
We will have 3 ESHA dietary datasets and one from NDSA. Currently, 2 ESHA datasets have been processed and we are waiting for 1 ESHA and one NSDA.
#### harmonization/p1_build_ESHA_csv.R
- Script for building tables from ESHA output - making columns from indention
- Meant to give a starting point to our harmonization dictionaries
- Combines ESHA from multiple projects into the same table
    - currently combines the MAP and MED studies
- Has unit test: harmonization/unit_tests/test_build_ESHA_csv.R
#### harmonization/p2_add_FPED_data.R
- Script for adding HEI/FPED categories to ESHA output
- Creates an HEI conversion table and adds HEI variables to the combined ESHA file.
- Has unit test: harmonization/unit_tests/test_add_FPED_data.R
#### harmonization/p3_automatic_label_proteins.R
##### Script for labeling the protein sources based off of rudimentary text mining and adding HEI values
###### rudimentary text mining
- Looks for key words in Item_name based off "dictoraries"
- Identifies protein sources and whether it is processed or not
- In script unit testing based on Lauren's hand processed script
###### Used the labeled proteins and the HEI conversion table to add HEI equivalants to combined ESHA reports
#### harmonization/p4_impute_missing_from_SR.R
##### Script for imputing missing nutrient data from best match in USDA's SR database
- SR_food.csv comes from the zip file loaded from https://fdc.nal.usda.gov/download-datasets.html
  - food.csv in FoodData_Central_sr_legacy_food_csv_2018-04.zip
  - accessed on 24 Feb 2024

### To run the workflow on SCINET HPC:
1. Load conda\
`module load miniconda`
2. Activate local [conda environment](workflow/env/python_conda_env.yml)\
`source activate python_ml_conda`
3. Launch snakemake:\
`snakemake --profile workflow/config`

## Developer notes
### Useful commands
conda env update --file workflow/env/python_conda_env.yml --prune

snakemake --dag |dot -Tpng > workflow/reports/dag.png

snakemake --rulegraph |dot -Tpng > workflow/reports/rulegraph.png

snakemake --debug-dag | dot -Tpdf > workflow/reports/debug_dag.pdf

snakemake --forceall --dag |dot -Tpdf > workflow/reports/forceall.pdf
--forceall 
 snakemake --forceall --dag | dot -Tpdf > dag.pdf

#### Remaining coding tasks:
* Finish normalization comparison plot
	1. Add pvalue tree	X
* Fix Sankey plots showing how each meat type aggrees by top level metabolites X
* Fix Sankey plots that show how sites compare for each meat type X
* Create Sankey plots showing highest score vs most versitile
	1. create new tables that have both highest score and most versitile
	2. run new tables through Sankey script
* Create Sankey plots showing processed verse other processed using most versitile site
	1. create new tables that have all processed
	2. run new tables through Sankey script
* Create Sankey plots showing min processed verse other min processed most versitile site
	1. create new tables that have all min processed
	2. run new tables through Sankey script
* Create Sankey plots showing each meat type vs each other meat type most versitile site X (for beef)
	1. create new tables that have meats
	2. run new tables through Sankey script
* Make rule for comparing response variables by site
* Add HEI calculating scripts and plots
