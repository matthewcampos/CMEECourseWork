##Homogenous and Heterogenous Fitness plots without migration

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

#For plotting different colours
library(RColorBrewer)

diff_rda_list <- list.files(path = "../Data/GNDIFF_50-65",full.names = TRUE)
diff_rda_list_2 <- list.files(path = "../Data/GNDIFF_50-80",full.names = TRUE)
same_rda_list <- list.files(path = "../Data/GNSAME_50-65",full.names = TRUE)
same_rda_list_2 <- list.files(path = "../Data/GNSAME_50-80",full.names = TRUE)
diff.rda <- diff_rda_list[c(1,12,23,34)]
diff.rda.2 <- diff_rda_list_2[c(1,12,23,34)]
same.rda <- same_rda_list[c(1,12,23,34)]
same.rda.2 <- same_rda_list_2[c(1,12,23,34)]
heterogenous <- c(diff.rda[c(1,2)],diff.rda.2[c(1,2)],same.rda[c(1,2)],same.rda.2[c(1,2)])
#fitness analysis- mean and SD
heterogenous.list <- list()
for (q in 1:length(heterogenous)){
    for (w in 1:10){
      load(paste0(heterogenous[q],'/run_1/Fitness/Simulation',w,'.rda'))
      heterogenous.list[[length(heterogenous.list)+1]] <- fit[c(80:700),1]
    }
}
heterogenous.fitness <- unlist(heterogenous.list)
homogenous.list <- list()
homogenous <- c(diff.rda[c(3,4)],diff.rda.2[c(3,4)],same.rda[c(3,4)],same.rda.2[c(3,4)])
for (z in 1:length(homogenous)){
    for (x in 1:10){
      load(paste0(homogenous[z],'/run_1/Fitness/Simulation',x,'.rda'))
      homogenous.list[[length(homogenous.list)+1]] <- fit[c(80:700),1]
    }
}
homogenous.fitness <- unlist(homogenous.list)
#Plots
plot.colour <- rainbow(30,start = 0, end = 0.25)
for (k in 1:8){
  path <- (paste0(heterogenous[k],'/run_1/Fitness/'))
  pdf(paste0("../Results/Early_Heterogenous_Plot_of_Simulations.pdf")) #open pdf
  for (j in 1:10){
    load(paste0(path,'Simulation',j,'.rda'))
    if (j == 1){
      plot(1:length(fit[,1]),fit[,1],type = 'l',xlab = 'Generations', ylab = 'Fitness',lty=j,col=plot.colour[j])
      mtext("Fitness of Heterogenous Population with no Migration",side = 3, line = -2, outer = TRUE)
    }else{
      lines(1:length(fit[,1]),fit[,1],type='l',lty=j,col=plot.colour[j])
    }
  }
  dev.off()
}
rm(k)
rm(j)
plot.colour <- rainbow(30,start = 0.7, end = 0.9)
for (k in 1:8){
  path <- (paste0(homogenous[k],'/run_1/Fitness/'))
  pdf(paste0("../Results/Early_Homogenous_Plot_of_Simulations.pdf")) #open pdf
  for (j in 1:10){
    load(paste0(path,'Simulation',j,'.rda'))
    if (j == 1){
      plot(1:length(fit[,1]),fit[,1],type = 'l',xlab = 'Generations', ylab = 'Fitness',lty=j,col=plot.colour[j],ylim = c(0,1))
      mtext("Fitness of Homogenous Population with no Migration",side = 3, line = -2, outer = TRUE)
    }else{
      lines(1:length(fit[,1]),fit[,1],type='l',lty=j,col=plot.colour[j])
    }
  }
  dev.off()
}



                