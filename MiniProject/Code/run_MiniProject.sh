#!/bin/sh

##Bash script to run entire MiniProject Scripts: Python, R script and LaTex files
##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

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
bash CompileLaTex.Sh MiniProject_report
#word count
texcount MiniProject_report.tex
#re-run with word count
bash CompileLaTex.Sh MiniProject_report

echo "Report Compiled"
