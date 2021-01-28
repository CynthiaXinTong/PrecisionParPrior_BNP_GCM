#!/bin/bash
#SBATCH -n 1
#SBATCH -t 03:00:00
#SBATCH -o analysis.res.out
#SBATCH -p standard
#SBATCH -A tongx

module load goolf R/4.0.0
Rscript analysis.res.R