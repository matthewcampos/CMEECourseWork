##Analysis

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

rm(list=ls()) #clear workspace
source('functions.R')
library(zeallot) #for assigning variables
#open data
gndiff.50.65.rda_list <- list.files(path = "../Results/DIFFGN_50-65",full.names = TRUE)
#gndiff.50.80.rda_list <- list.files(path = "../Results/GNDIFF_50-80",full.names = TRUE)
#gnsame.50.65.rda_list <- list.files(path = "../Results/GNSAME_50-65",full.names = TRUE)
#gnsame.50.80.rda_list <- list.files(path = "../Results/GNSAME_50-80",full.names = TRUE)

condition <- c('No Migration', 'Random Migration', 'Migration 1%', 'Migration 3%', 'Migration 5%','Migration 1% Every 5 Gen.','Migration 1% Every 10 Gen.','Migration 3% Every 5 Gen.','Migration 5% Every 5 Gen.','Migration 3% Every 10 Gen.','Migration 5% Every 10 Gen.')
gene.condition <- rep(condition, times=4)
gene_title <- c("Heterogenous-Heterogenous","Heterogenous-Homogenous","Homogenous-Heterogenous","Homogenous-Homogenous")
rep.gene.title <- rep(gene_title,each=length(condition))
title <- paste(rep.gene.title, gene.condition)


load("../Results/DIFFGN_50-65/heterozygous-heterozygous_Migration_0.5-1/run_1/Traits/Simulation15.rda")
load("../Results/DIFFGN_50-65/heterozygous-heterozygous_Migration_0.5-1/run_1/Fitness/Simulation15.rda")
recovery.list <- list()
#how many generations to recover with migration
avg_fit <- mean(fit[c(70:80),1]) #just before migration
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
avg_recovery_time <- mean(recovery_time)

#are any migrant alleles maintained- how long do they last at the 700th generation

#speed to reach max 
max_speed <- which(fit[,1]>=avg_fit)[1]

#migration rates make best genotype more robust- before migration and after
#       -calculate trait value of best individual
#       -replicate it to 100 individuals
#       -set of mutations at each allele site- single mutants, double, triple 
#       -calculate trait value and fitness
#       -measuring variance of the trait values 
#prediction is that individuals in the beginning should be less robust than the ones at the end 
y3 <- yvalues.list[[3]][1200]
y3.max <- which.max(y3[[1]]) #max individual
ind <- pop.list[[length(pop.list)]][y3.max,,]
pop.1 <- population(size=13,locus) #for mutations in 1st strand
pop.2 <- population(size=13,locus) #mutations in 2nd strand
#replicate individual
for (person  in 1:dim(pop)[1]){
  pop.1[person,,] <- ind
  pop.2[person,,] <- ind
}
#mutation per site
for (site in 1:dim(pop)[2]){
  pop.1[site+1,site,1] <- as.numeric(pop.1[site+1,site,1]) - 0.001
  pop.2[site+1,site,2] <- as.numeric(pop.2[site+1,site,2]) - 0.001
}
#calculate y_values
mu.1 <- mu_values(pop.1[,9,],pop.1[,10,]) #mu of individuals with mutations in 1st strand
mu.2 <- mu_values(pop.2[,9,],pop.2[,10,]) #mu of individuals with mutations in 2nd strand
y_3 <- (mu$`31` * positive_R_j(y_j$`1`,theta$`31`,P$`31`)) + (mu$`32` * positive_R_j(y_j$`1`,theta$`32`,P$`32`))
#combination of anova and regression
#dataframe to save all those above  
main.pop.title <- rep(c('Heterogenous','Homogenous'),each=22)
migrant.pop.title <- rep(c('Heterogenous',"Homogenous",'Heterogenous',"Homogenous"),each=11)
migration.rate <- rep(c(0,-1,1,3,5,1,1,3,5,3,5),times=4)
migration.pattern <- rep(c(0,3,0,0,0,2,1,2,2,1,1),times=4)
result <- data.frame(main.pop.title,migrant.pop.title,migration.rate,migration.pattern,stringsAsFactors = FALSE)


