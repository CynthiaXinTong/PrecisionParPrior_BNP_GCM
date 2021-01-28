The simulation study was conducted using high performance computing clusters. The .slurm files were used to submit jobs to the clusters.

1. generate.R is used to generate data for the simulation study.

2. analysis.R is a sample code for analyzing data using the BNP growth curve modeling. model1.txt - model4.txt are JAGS code for BNP growth curve model specification with four precision parameter priors, respectively.

3. sim_script.R is used to modify analysis.R (i.e., change working directories and datafile names) and generate an R file for each simulation condition.

4. res.R is used to summarize simulation results.

5. convergence.R is used to obtain model convergence rates.