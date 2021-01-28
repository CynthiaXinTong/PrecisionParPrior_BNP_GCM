#!/bin/bash
#SBATCH -n 1
#SBATCH -t 07:00:00
#SBATCH --array=1-8000
#SBATCH --partition=standard
#SBATCH -A tongx

module load jags
module load goolf R/4.0.0
Rscript R${SLURM_ARRAY_TASK_ID}.R
