# README Project
<ul>Contains R script that create simulation and conditions, analysis, bash script to run different permutations and data of runs

##Directory
**Code**- this consists of the R scripts and bash scripts to run simulations and analyse them
#####R script:
* functions.R- script of functions used in simulation to generate populations and simulate evolutionary processes on the populations
* simulation(1:4).R- main script of the simulation that sources the functions script to run simulations. The variants of the script are because of different environmental distances and genetic makeup of the migrant population.
* analysis.R- script that gets data from Data Folder and does regression and ANOVA analysis. Creates the dataframes containing conditions, recovery results, speed to max fitness results and ratio for robustness results from the data, and uses them to perform the regression and ANOVA analysis to generate results.
* Migration_plots.R- produces the fitness graphs to see the effects of migration rates and patterns, and genetic makeup have on the evolution of fitness of the genetic network. There should be 4 graphs produced each containing 44 lines in a single plot
* No_migration_plots.R- produces fitness graphs of the conditions where there was no migration to see the speed at which the network evolves depending on genetic makeup at the start.
#####Bash script
* a_sim_(1:4)-(1:4).sh- bash script to run the simulation scripts of the different starting conditions: starting population size, genetic makeup of both populations, migration rates and migration patterns
#####LaTex
* Sections
    - Introduction.tex- tex file of the introduction section for thesis
    - Methods.tex- tex file of the methods section of the thesis
    - Results.tex- tex file of the results section
    - Discussion.tex- tex file of discussion of results
    - Conclusion.tex- tex file of conclusion
* references.bib- bibtex file containing the references used in the report
* CompileLaTex.sh- convert the tex files to generate thesis pdf in the Results folder
* thesis.tex- tex file that produces the whole report taking in tex files from Sections folder, references.bib and Titlepage.tex
* Titlepage.tex- tex file to produce title page
#####Unix
* UnixPrac1.txt- first practical which analyses three files from Data directory

**Proposal**
* folder containing proposal for thesis project 

**Data**- this directory consists of the data used to run the 44 conditions in the code
* GNDIFF_50-65
* GNDIFF_50-80
* GNSAME_50-65
* GNSAME_50-65
* In each of the folder it contains the following:
  - Folders of 44 combinations of starting population size, genetic makeup of both populations, migration rates and migration patterns
  - Folder of the number of runs containing the number of simulations per run
  - Fitness folder- contains rda files of the saved fitness array (fit) of each generation from the simulations
  - Population folder- rda files of the saved population arrays (both main and migrant) as a list for the simulations
  - Traits folder- rda files of the saved trait values (as a list) of each generation for each of the simulation
  - Parents folder- REMOVE COMMENT ON LINE 305 & 306 AND RUN SIMULATION SCRIPTS AS NOT USED IN CURRENT DATA ANALYSIS- contains rda files of parent combinations for each generation per simulation.
  - Alleles folder- REMOVE COMMENT ON LINE 307 & 308 AND RUN SIMULATION SCRIPTS AS NOT USED IN CURRENT DATA ANALYSIS- rda file containing the sites that mutated for each generation per simulation
  - Migrants folder- REMOVE COMMENT ON LINE 309 & 310 AND RUN SIMULATION SCRIPTS AS NOT USED IN CURRENT DATA ANALYSIS- rda

**Results**


**Sandbox**
*Cauchy_test.R- visualise cauchy distribution
