##Analysis using regression and ANOVA

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

rm(list=ls()) #clear workspace
source('functions.R')
library(dplyr)

#open data
gndiff.50.65.rda_list <- list.files(path = "../Data/GNDIFF_50-65",full.names = TRUE)
gndiff.50.80.rda_list <- list.files(path = "../Data/GNDIFF_50-80",full.names = TRUE)
gnsame.50.65.rda_list <- list.files(path = "../Data/GNSAME_50-65",full.names = TRUE)
gnsame.50.80.rda_list <- list.files(path = "../Data/GNSAME_50-80",full.names = TRUE)
folder.list <- list(gndiff.50.65.rda_list,gndiff.50.80.rda_list,gnsame.50.65.rda_list,gnsame.50.80.rda_list)

#Recovery time of average fitness of runs
#matrix to save each individual run result
maxspeed.recovery.matrix <- matrix(NA,nrow = 2640,ncol = 4)
colnames(maxspeed.recovery.matrix) <- c('avg','sd','max speed','avg_fit')
for (i in 1:4){
  rda_list <- folder.list[[i]]
  recovery.list <- list()
  recovery.sd.list <-list()
  maxspeed.list <- list()
  maxspeed.val.list <- list()
  for (j in 1:44){
    persim.recovery.list <- list()
    persim.recovery.sd.list <-list()
    persim.maxspeed.list <- list()
    persim.maxspeed.val.list <- list()
    for (k in 1:15){
      load(paste0(rda_list[j],'/run_1/Fitness/Simulation',k,'.rda'))
      avg_fit <- mean(fit[c(70:79),1]) #just before migration
      max_speed <- which(fit[,1]>=avg_fit[1]) #speed to reach max
      mig_effect <- which(fit[,1]<avg_fit) #less than avg
      mig_effect <- mig_effect[-which(mig_effect < 80 | mig_effect > 700)] #within migration generations only
      recover <- which(fit[,1]>=avg_fit)
      recover <- recover[-which(recover < 80 | recover > 700)] #within migration generations only
      recovery_time <- c()
      count <- 0 
      for (l in 80:700){
        if (length(which(mig_effect==l))>0){
          count <- count + 1 #every instance fitness is below avg 
        }else{
          recovery_time <- c(recovery_time,count)
          count <- 0
        }
      }
      if (is.null(recovery_time)==TRUE){
        recovery_time <- c(0,0)
      }
      persim.recovery.list[[length(persim.recovery.list)+1]] <- mean(recovery_time)
      persim.recovery.sd.list[[length(persim.recovery.sd.list)+1]] <- sd(recovery_time)
      persim.maxspeed.list[[length(persim.maxspeed.list)+1]] <- as.numeric(which(fit[,1]>=avg_fit)[1])
      persim.maxspeed.val.list[[length(persim.maxspeed.val.list)+1]] <- avg_fit
    }
    recovery.list[[length(recovery.list)+1]] <- persim.recovery.list
    recovery.sd.list[[length(recovery.sd.list)+1]] <- persim.recovery.sd.list
    maxspeed.list[[length(maxspeed.list)+1]] <- persim.maxspeed.list
    maxspeed.val.list[[length(maxspeed.val.list)+1]] <- persim.maxspeed.val.list
  }
  #saves matrix of the 4 environmental/genetic conditions
  if (i == 1){
    maxspeed.recovery.matrix[1:660,1] <- as.numeric(unlist(persim.recovery.list)) 
    maxspeed.recovery.matrix[1:660,2] <- as.numeric(unlist(persim.recovery.sd.list))
    maxspeed.recovery.matrix[1:660,3] <- as.numeric(unlist(persim.maxspeed.list))
    maxspeed.recovery.matrix[1:660,4] <- as.numeric(unlist(persim.maxspeed.val.list))
  }else if (i == 2){
    maxspeed.recovery.matrix[661:1320,1] <- as.numeric(unlist(persim.recovery.list)) 
    maxspeed.recovery.matrix[661:1320,2] <- as.numeric(unlist(persim.recovery.list))
    maxspeed.recovery.matrix[661:1320,3] <- as.numeric(unlist(persim.maxspeed.list))
    maxspeed.recovery.matrix[661:1320,4] <- as.numeric(unlist(persim.maxspeed.val.list))
  }else if (i == 3){
    maxspeed.recovery.matrix[1321:1980,1] <- as.numeric(unlist(persim.recovery.list)) 
    maxspeed.recovery.matrix[1321:1980,2] <- as.numeric(unlist(persim.recovery.list))
    maxspeed.recovery.matrix[1321:1980,3] <- as.numeric(unlist(persim.maxspeed.list))
    maxspeed.recovery.matrix[1321:1980,4] <- as.numeric(unlist(persim.maxspeed.val.list))
  }else if (i == 4){
    maxspeed.recovery.matrix[1981:2640,1] <- as.numeric(unlist(persim.recovery.list)) 
    maxspeed.recovery.matrix[1981:2640,2] <- as.numeric(unlist(persim.recovery.list))
    maxspeed.recovery.matrix[1981:2640,3] <- as.numeric(unlist(persim.maxspeed.list))
    maxspeed.recovery.matrix[1981:2640,4] <- as.numeric(unlist(persim.maxspeed.val.list))
  }
}

#Before and after migration variance ratio for robustness
all.ratio.matrix <- matrix(NA,nrow = 2640,ncol = 1)
colnames(all.ratio.matrix) <- 'Ratio'
for (m in 1:4){
  check <- 0
  rda_list <- folder.list[[m]]
  ratio.list <- list()
  for (n in 1:44){
    check <- check + 1
    print(check)
    check.2 <- 0
    ratio.matrix <- matrix(NA,nrow = 15,ncol = 1)
    for (p in 1:15){
      check.2 <-check.2 + 1
      print(check.2)
      load(paste0(rda_list[n],'/run_1/Traits/Simulation',p,'.rda'))
      load(paste0(rda_list[n],'/run_1/Population/Simulation',p,'.rda'))
      #before migration trait values
      bm.y3 <- yvalues.list[[3]][80] 
      bm.y3.max <- which.max(bm.y3[[1]])
      bm.ind <- pop.list[[2]][bm.y3.max,,] #most fit before migration
      bm.pop.1 <- population(size=13,locus=12) #for mutations in 1st strand
      bm.pop.2 <- population(size=13,locus=12) #mutations in 2nd strand
      bm.pop <- population(size=25,locus = 12) #combine both populations to calculate yvalues
      bm.y1.list <- yvalues.list[[1]][80]
      bm.y1.val <- bm.y1.list[[1]][bm.y3.max]
      bm.y2.list <- yvalues.list[[2]][80]
      bm.y2.val <- bm.y2.list[[1]][bm.y3.max]
      #after migration trait values
      am.y3 <- yvalues.list[[3]][710] 
      am.y3.max <- which.max(am.y3[[1]]) #max individual
      am.ind <- pop.list[[13]][am.y3.max,,] #most fit after migration
      am.pop.1 <- population(size=13,locus=12) #for mutations in 1st strand
      am.pop.2 <- population(size=13,locus=12) #mutations in 2nd strand
      am.pop <- population(size=25,locus = 12)
      am.y1.list <- yvalues.list[[1]][710]
      am.y1.val <- am.y1.list[[1]][am.y3.max]
      am.y2.list <- yvalues.list[[2]][710]
      am.y2.val <- am.y2.list[[1]][am.y3.max]
      #replicate individual
      for (person  in 1:dim(am.pop.1)[1]){
        am.pop.1[person,,] <- am.ind #replicates fit individual for mutation
        am.pop.2[person,,] <- am.ind
        bm.pop.1[person,,] <- bm.ind
        bm.pop.2[person,,] <- bm.ind
      }
      #mutation per site
      for (site in 1:dim(am.pop.1)[2]){
        am.pop.1[site+1,site,1] <- as.numeric(am.pop.1[site+1,site,1]) - 0.0005 #mutation each site
        am.pop.2[site+1,site,2] <- as.numeric(am.pop.2[site+1,site,2]) - 0.0005
        bm.pop.1[site+1,site,1] <- as.numeric(bm.pop.1[site+1,site,1]) - 0.0005
        bm.pop.2[site+1,site,2] <- as.numeric(bm.pop.2[site+1,site,2]) - 0.0005
      }
      #before migration- combining population
      bm.pop[c(1:13),,] <- bm.pop.1
      bm.pop.2 <- bm.pop.2[-1,,] #remove first individual without mutation as already in population
      bm.pop[c(14:25),,] <- bm.pop.2
      #after migration
      am.pop[c(1:13),,] <- am.pop.1
      am.pop.2 <- am.pop.2[-1,,]
      am.pop[c(14:25),,] <- am.pop.2
      #calculate trait values
      #Before Migration
      bmy_1 <- (mu_values(bm.pop[,1,1],bm.pop[,2,1]) * negative_R_j(bm.y2.val,bm.pop[,7,1],bm.pop[,8,1])) + (mu_values(bm.pop[,1,2],bm.pop[,2,2]) * negative_R_j(bm.y2.val,bm.pop[,7,2],bm.pop[,8,2]))
      bmy_2 <- (mu_values(bm.pop[,5,1],bm.pop[,5,1]) * positive_R_j(bm.y1.val,bm.pop[,3,1],bm.pop[,4,1])) + (mu_values(bm.pop[,5,2],bm.pop[,5,2]) * positive_R_j(bm.y1.val,bm.pop[,3,2],bm.pop[,4,2]))
      bmy_3 <- (mu_values(bm.pop[,9,1],bm.pop[,9,1]) * positive_R_j(bmy_1,bm.pop[,11,1],bm.pop[,12,1])) + (mu_values(bm.pop[,9,2],bm.pop[,9,2]) * positive_R_j(bmy_1,bm.pop[,11,2],bm.pop[,12,2]))
      #After Migration
      amy_1 <- (mu_values(am.pop[,1,1],am.pop[,2,1]) * negative_R_j(am.y2.val,am.pop[,7,1],am.pop[,8,1])) + (mu_values(am.pop[,1,2],am.pop[,2,2]) * negative_R_j(am.y2.val,am.pop[,7,2],am.pop[,8,2]))
      amy_2 <- (mu_values(am.pop[,5,1],am.pop[,5,1]) * positive_R_j(am.y1.val,am.pop[,3,1],am.pop[,4,1])) + (mu_values(am.pop[,5,2],am.pop[,5,2]) * positive_R_j(am.y1.val,am.pop[,3,2],am.pop[,4,2]))
      amy_3 <- (mu_values(am.pop[,9,1],am.pop[,9,1]) * positive_R_j(amy_1,am.pop[,11,1],am.pop[,12,1])) + (mu_values(am.pop[,9,2],am.pop[,9,2]) * positive_R_j(amy_1,am.pop[,11,2],am.pop[,12,2]))
      #Get fitness from trait
      bm.fitness <- fitness(bmy_3,50,8)
      am.fitness <- fitness(amy_3,50,8)
      #ratio
      ratio.matrix[p,1] <- sd(am.fitness) / sd(bm.fitness)
    }
    ratio.list[[length(ratio.list)+1]] <- ratio.matrix
  }
  if (m == 1){
    all.ratio.matrix[1:660,1] <- as.numeric(unlist(ratio.list)) #saves matrices of the 44 conditions of the 4 environment/genetic conditions
  }else if (m == 2){
    all.ratio.matrix[661:1320,1] <- as.numeric(unlist(ratio.list)) 
  }else if (m == 3){
    all.ratio.matrix[1321:1980,1] <- as.numeric(unlist(ratio.list)) 
  }else if (m == 4){
    all.ratio.matrix[1981:2640,1] <- as.numeric(unlist(ratio.list)) 
  }
}

#combination of anova and regression
#dataframe to save all those above  
main.pop.title <- rep(c('Heterogenous','Homogenous'),each=88)
migrant.pop.title <- rep(c('Heterogenous',"Homogenous",'Heterogenous',"Homogenous"),each=44)
migration.rate <- rep(c(0,-1,1,3,5,1,1,3,5,3,5),times=16)
migration.pattern <- rep(c(0,3,0,0,0,2,1,2,2,1,1),times=16)
result <- data.frame(main.pop.title,migrant.pop.title,migration.rate,migration.pattern,all.ratio.matrix,maxspeed.recovery.matrix)

#fitness variation during migration periods
wmigration.fitness.list <- list()
womigration.fitness.list <- list()
for (v in 1:4){
  list <- folder.list[[v]]
  migration.rda <- list[-c(1,12,23,34)]
  for (z in 1:length(migration.rda)){
    for (x in 1:15){
      load(paste0(migration.rda[z],'/run_1/Fitness/Simulation',x,'.rda'))
      wmigration.fitness.list[[length(wmigration.fitness.list)+1]] <- fit[c(80:700),1]
    }
  }
}
wmigration.fitness <- unlist(wmigration.fitness.list)
for (a in 1:4){
  list <- folder.list[[a]]
  womigration.rda <- list[c(1,12,23,34)]
  for (q in 1:length(womigration.rda)){
    for (w in 1:15){
      load(paste0(womigration.rda[q],'/run_1/Fitness/Simulation',w,'.rda'))
      womigration.fitness.list[[length(womigration.fitness.list)+1]] <- fit[c(80:700),1]
    }
  }
}
womigration.fitness <- unlist(womigration.fitness.list)
#boxplot to compare migration and w/o migration
wo.migration <- data.frame(result$migration.rate,result$migration.pattern,result$max.speed,result$avg_fit)
rm_migration <- which(wo.migration$result.migration.rate==0 & wo.migration$result.migration.pattern==0)
wo.migration <- wo.migration[rm_migration,] #remove migration
w.migration <- data.frame(result$migration.rate,result$migration.pattern,result$max.speed,result$avg_fit)
rm_nomigration <- which(w.migration$result.migration.rate==0 & w.migration$result.migration.pattern==0)
w.migration <- w.migration[-rm_nomigration,] #remove no migration
matrix.both <- as.data.frame(matrix(NA,nrow = 2400*2,ncol = 3)) #for box plot
matrix.both[1:2400,1] <- "Without Migration"
matrix.both[2401:4800,1] <- "With Migration"
matrix.both[1:2400,2] <- as.numeric(wo.migration$result.max.speed)
matrix.both[2401:4800,2] <- as.numeric(w.migration$result.max.speed)
matrix.both[1:2400,3] <- as.numeric(wo.migration$result.avg_fit)
matrix.both[2401:4800,3] <- as.numeric(w.migration$result.avg_fit)
matrix.both$V1 <- factor(matrix.both$V1, levels = c('Without Migration','With Migration'))
boxplot(matrix.both$V2~matrix.both$V1,data = matrix.both,xlab = 'Conditions',ylab = 'Speed to Average Fitness')

#Migration rate and pattern on recovery rate
migration.result <- data.frame(result$migration.rate,result$migration.pattern,result$avg,result$sd)
migration.result <- migration.result[-which(migration.result$result.migration.rate==0 & migration.result$result.migration.pattern==0),]

