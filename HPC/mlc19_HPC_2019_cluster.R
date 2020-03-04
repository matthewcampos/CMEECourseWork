# CMEE 2019 HPC excercises R code HPC run code proforma
#Question 18
rm(list=ls()) # good practice 
graphics.off()
source("mlc19_HPC_2019_main.R")
#HPC Cluster
iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))
#Local test Cluster
#iter <- seq(3) #testing first three 
#set.seed(as.integer(iter))
#mod <- iter %% 4
#speciation_rate <- 0.003874
#wall_time <- 11.5 * 60
#interval_rich <- 1
set.seed(as.integer(iter))
mod <- iter %% 4
speciation_rate <- 0.003874
wall_time <- 11.5 * 60
interval_rich <- 1
if (mod == 1){
  size = 500
} else if (mod == 2){
  size = 1000
} else if (mod == 3){
  size = 2500
} else{
  size = 5000
}
output_file_name <- paste("Cluster_run_iteration",iter,".rda",sep = "_")
cluster_run(speciation_rate, size, wall_time, interval_rich, interval_oct = size/10, burn_in_generations = 8*size, output_file_name)


