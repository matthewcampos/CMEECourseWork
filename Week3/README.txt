# README Week 3
<ul>The week focused on learning R and Data Management, exploration and visualisation, for the purpsoe of data analysis.

##Directory
**Code**
* basic_io.R- script to practice input and output of data
* control_flow.R- practice with if, for and while loops in R
* break.R- using break to stop loops
* next.R- using next in loops
* boilerplate.R- understanding syntax of writing a function in R
* Vectorize1.R- comparing loops with optimised code on a vector
* preallocate.R- understanding how memory allocation works
* apply1.R- collection of functions that vectorize code for you
* apply2.R- constructed functions used for vectorization
* sample.R- practice using lapply and sapply
* browse.R- understanding how to debug in R
* DataWrang.R- script that data wrangles PoundHillData.csv in Data directory
* Girko.R- plots two dataframes using Girko's circular law
* MyBars.R- using geom and text in ggplot to annotate a plot
* plotLin.R- mathematical annotations on a axis

**Practicals**
* TreeHeight.R- function that calculates tree heights and modified to take in data and print results in a separate file in Results directory
* Vectorize2.R- vectorization challenge to improve run time of a script
* Vectorize1.py & Vectorize2.py- extra credit of translating Vectorize1.R & Vectorize2.R into python script and showing run time through bash script
* run_Vectorize.sh - bash script to run the Vectorize practicals in R and Python
* TAAutoCorr.R- script to determine if there is correlation in weather data
* autocorrelation.tex- LaTeX document of analysis of TAAutoCorr.R. PDF version saved in Results directory
* get_TreeHeight.R- extra credit of taking input of file name from command line
* get_TreeHeight.py- code of get_TreeHeight.R written in python syntax
* run_get_TreeHeight.sh- bash script to run get_TreeHeight.R and get_TreeHeight.py with trees.csv from Data directory as input
* DataWrangTidy.R- using dplyr and tidyr to wrangle data
* PP_Latice.R- script that produces three different graphs using data from EcolArchives-E089-51-D1.csv and outputs in three separate files in Results directory. Also produces data frame of quantitative results and writes it in PP_Results.csv
* PP_Regress.R- script to plot regression of predator mass and prey mass, subsetting by predator.lifestage and Type.of.feeding.interaction. Calculated regression results saved in PP_Regress_Results.csv in Results directory
* PP_Regress_loc.R- calculates regression but combines Type.of.feeding.interaction, Predator.lifestage, and Location
* GPDD_Data.R- utlises map package and plots location data (longtitude and latitude) in map

**Data**
* EcolArchives-E089-51-D1.csv
* GPDDFiltered.RData
* KeyWestAnnualMeanTemperature.RData
* PoundHillData.csv
* PoundHillMetaData.csv
* trees.csv

**Results**
* trees_treeheight.csv- ran using R code
* Py_trees_treeheight.csv- ran using Python code
* autocorrelation.PDF
* PP_Regress_loc.csv
* TAAutoCorrRplot.pdf
* PP_Regress.csv
* PP_regress.pdf
* Pred_Lattice.pdf
* PP_Results.csv
* SizeRatio_Lattice.pdf
* Prey_Lattice.pdf
* TreeHts.csv
* MyData.csv
* Girko.pdf
* MyBars.pdf
* MyLinReg.pdf

**Sandbox**
* Notes.R
