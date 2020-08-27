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

**Proposal**
* folder containing proposal for thesis project

**Data**- this directory consists of the data used to run the 44 conditions in the code.
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
 * To run more simulations per folder, use the following combination of conditions (44 conditions total):
   - Heterozygous - Heterozygous, Migration rate (0%,1%,3%,5%), Migration patterns (0 (every gen.),1 (every 10 gen.),2 (every 5 gen.))
   - Heterozygous - Homozygous, Migration rate (0%,1%,3%,5%), Migration patterns (0 (every gen.),1 (every 10 gen.),2 (every 5 gen.))
   - Homozygous - Heterozygous, Migration rate (0%,1%,3%,5%), Migration patterns (0 (every gen.),1 (every 10 gen.),2 (every 5 gen.))
   - Homozygous - Homozygous, Migration rate (0%,1%,3%,5%), Migration patterns (0 (every gen.),1 (every 10 gen.),2 (every 5 gen.))

**Results**
* Early_Homogenous_Plot_of_Simulation.pdf- plotting fitness of homogenous populations evolving without migration
* Early_Heterogenous_Plot_of_Simulation.pdf- plotting fitness of heterogenous populations evolving without migration
* workflow.jpg- image file of the workflow of the simulation for the Appendix
* robustness_regression.jpg- image file of the regression result of calculated Robustness ratio
* boxplot_recovery_time.jpg- image file of boxplot to compare recovery times per condition
* robustness_anova.pdf- pdf file of table used to show anova of regression results
* AppendixI_conditions.jpg- image file of the conditions used as input arguments for simulation
* workflow.jpg- image file of workflow of simulation
* Campos_Matthew_CMEE_2020.pdf- pdf of thesis
* tukey_anova.pdf- pdf of post-hoc tukey test result after anova analysis
* bartlett_anova.pdf- pdf of bartlett test to conduct anova test on regression
* qq_plot.jpg- image file of Q-Q plot for anova test
* robustness_anova.pdf- anova result of regression test of migration pattern effect on robustness ratio
* Migration_Fitness_plots- folder containing pdf fitness plots for the 4 different genetic variation and environmental distance combinations
  - GNDIFF_50-65
    - Plot_of_simulations.pdf
  - GNDIFF_50-80
  - GNSAME_50-65
  - GNSAME_50-65
* Cauchy_distribution.pdf- pdf showing the cauchy distribution for determining fitness values from trait values
* Model_diagram.jpg- image file of the two variants of migrant genetic networks

**Sandbox**
* Cauchy_test.R- visualise cauchy distribution
* environment.RData- R environment used for analysis
