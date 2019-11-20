# README Week 7
<ul>The week focused on learning other Python components including numerical computing, Regex and data visualisation.

##Directory
**Code**
* profileme.py- functions of for loops that are used to test run -p on ipython (check run time)
* profileme2.py-same function but a list comprehension again checking run time
* timeitme.py- script that uses timeit rather than run -p to check the run time of individual functions
* Nets.R- produces a network to visualise the QMEE CDT collaboration network
* regexs.py- practice with regex and using different combination of elements to understand output
* TestR.R- script to be tested if it can be opened in workflow
* TestR.pyscript to test workflow and open the R script using subprocess
* fmr.R- reads NagyEtAl1999.csv and produces a graph

**Practicals**
* LV1.py- created a self-standing script that generated plots of consumer-resource population dynamics using scipy, using the Lotka-Volterra model
* LV2.py- Lotka-Volterra model however takes values of input variables from command line
* LV3.py- Lotka-Volterra model however where time increments are discrete and does not utilise scipy integration
* LV4.py- same as LV3.py except includes a random gaussian fluctuation drawn each iteration
* run_lv.py- script to run the different LV practicals
* DrawFW.py- script to construct a foodweb using networkx, saving graph in Results directory
* Nets.py- converts Nets.R into python script utilising networkx and matplotlib and saves results in Results directory
* blackbirds.py- reads the file blackbirds.txt and extracts certain statements using regex commands
* using_os.py- collects files and directories from directory based on certain conditions
* run_fmr_R.py- workflow to run fmr.R and produce output on screen and save graph in Results directory

**Data**
* blackbirds.txt
* NagyEtAl1999.csv
* QMEE_Net_Mat_edges.csv
* QMEE_Nat_Mat_nodes.csv
* TestOaksData.csv

**Results**
* LV_model.pdf
* LV_second_model.pdf
* LV2_model.pdf
* LV2_second_model.pdf
* LV3_model.pdf
* LV3_second_model.pdf
* LV4_model.pdf
* LV4_second_model.pdf
* QMEENet.svg
* Nets_py.svg
* DrawFW_network_model.pdf
* errorFile.Rout
* outputFile.Rout
* fmr_plot.pdf


**Sandbox**
