#!/bin/bash

##SBATCH --partition=Pisces                             # Partition/queue requested on server
#SBATCH --job-name=rf_auto_protein                      # Job name
#SBATCH --time=48:00:00                                 # Time limit (hrs:min:sec)
#SBATCH --nodes=1                                       # Number of nodes requested
#SBATCH --ntasks-per-node=1                             # Number of CPUs (processor cores/tasks)
#SBATCH --mem=100gb                                     # Memory limit
#SBATCH --mail-type=BEGIN,END,FAIL                      # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=aaron.yerke@usda.gov                # Specified email address
#SBATCH --output=./slurmLogs/%x.%j.log                  # Set directory for standard output
#SBATCH --error=./slurmLogs/%x.%j.log                   # Set directory for error log


### Display the job context
echo Job: $SLURM_JOB_NAME with ID $SLURM_JOB_ID
echo Running on host: `hostname`
echo Using $SLURM_NTASKS processors across $SLURM_NNODES nodes

module purge  # clear module environment
module load miniconda  # load miniconda
source activate python_ml_conda
python --version
python scripts/ml/random_forest.py \
	--pred_path data/metabolomics/demo-log-filt_all_bat_norm_imput-super_pathway.csv\
	--output_label demo-log-filt_all_bat_norm_imput-super_pathway-auto_protein\
	--response_fn data/mapping/rf_noMap_auto_protn_metadata.csv\
	--out_folder no_map_auto_protein \
	--delimiter , \
	--title "demo-log-filt_all_bat_norm_imput-super_pathway-auto_protein"\
	--id_var "PARENT_SAMPLE_NAME"

