#!/bin/bash

#SBATCH --partition=ceres                               # Partition/queue requested on server
#SBATCH --job-name=AY_SNAKEMAKE                         # Job name
#SBATCH --time=24:00:00                                 # Time limit (hrs:min:sec)
#SBATCH --nodes=1                                       # Number of nodes requested
#SBATCH --ntasks-per-node=1                          	# Number of CPUs (processor cores/tasks)
#SBATCH --mem=16gb                                      # Memory limit
#SBATCH --account=nhanes_ml
#SBATCH --mail-type=BEGIN,END,FAIL                      # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=aaronyerke@gmail.com                # Specified email address
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
snakemake --profile workflow/config 

echo ""
echo "======================================================"
echo "End Time   : $(date)"
echo "======================================================"