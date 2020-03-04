#!/bin/sh
#Bash script to run entire MiniProject
#Python, R script and LaTex files
#Matthew Campos
#04/03/2020

#Run Data Exploration
python3 "Data_wrangling.py"

#Run Data filtering
Rscript "Data_sort.R"

#Run equations functions
Rscript "equations.R"

#Run parameters functions
Rscript "parameters.R"

#Run NLS Fitting
Rscript "NLM_Fit.R"

#Run additional investigation
Rscript "Temp_corr.R"

#Run LaTex
bash CompileLaTex.Sh Thesis
#word count
texcount -1 -sum Thesis.tex > MiniProject.sum
#re-run with word count
bash CompileLaTex.Sh Thesis

echo "Report Compiled"
