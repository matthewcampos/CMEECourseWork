##Analysis

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

rm(list=ls()) #clear workspace
#open data
rda_list <- list.files(path = "../Results/DIFFGN_50-80",full.names = TRUE)
for (l in 1:44){
  fit_plot <- dir.create(paste0(rda_list[l],'/','Fit_Plots'), recursive = TRUE)
}
condition <- c('No Migration', 'Random Migration', 'Migration 1%', 'Migration 3%', 'Migration 5%','Migration 1% Every 5 Gen.','Migration 1% Every 10 Gen.','Migration 3% Every 5 Gen.','Migration 5% Every 5 Gen.','Migration 3% Every 10 Gen.','Migration 5% Every 10 Gen.')
gene.condition <- rep(condition, times=4)
gene_title <- c("Heterogenous-Heterogenous","Heterogenous-Homogenous","Homogenous-Heterogenous","Homogenous-Homogenous")
rep.gene.title <- rep(gene_title,each=length(condition))
title <- paste(rep.gene.title, gene.condition)
#plot fitness
for (k in 1:44){
  path <- (paste0(rda_list[k],'/run_1/Fitness/'))
  for (j in 1:30){
    load(paste0(path,'Simulation',j,'.rda'))
    pdf(paste0(rda_list[k],'/Fit_plots/',"Plot_of_Simulations_",j,".pdf")) #open pdf
    plot(1:length(fit[,1]),fit[,1],type = 'l',xlab = 'Generations', ylab = 'Fitness', main = title[k])
    dev.off()
  }
}

load("../Results/DIFFGN_50-80/heterozygous-heterozygous_Migration_0-0/run_1/Traits/Simulation15.rda")
#how many generations to recover with migration
mig_effect <- which(fit[,1]<avg_fit) #less than avg
mig_effect <- mig_effect[-which(mig_effect < 80 | mig_effect > 700)] #within migration generations only
recover <- which(fit[,1]>=avg_fit)
recover <- recover[-which(recover < 80 | recover > 700)] #within migration generations only
recovery_time <- c()
count <- 0 
for (i in 80:700){
  if (length(which(mig_effect==i))>0){
    count <- count + 1 #every instance fitness is below avg 
  }else{
    recovery_time <- c(recovery_time,count)
    count <- 0
  }
}
mean(recovery_time)

#are any migrant alleles maintained- how long do they last at the 700th generation

#speed to reach max 
max_speed <- which(fit[,1]>=max(fit[,1]))[1]

#migration rates make best genotype more robust- before migration and after
#       -calculate trait value of best individual
#       -replicate it to 100 individuals
#       -set of mutations at each allele site- single mutants, double, triple 
#       -calculate trait value and fitness
#       -measuring variance of the trait values 
#prediction is that individuals in the beginning should be less robust than the ones at the end 
y3 <- yvalues.list[[3]][1200]
which.max(y3[[1]]) #max individual


