##Homogenous and Heterogenous Fitness plots without migration

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

#For plotting different colours
library(RColorBrewer)

diff_rda_list <- list.files(path = "../Results/DIFFGN_50-65",full.names = TRUE)
diff_rda_list_2 <- list.files(path = "../Results/DIFFGN_50-80",full.names = TRUE)
same_rda_list <- list.files(path = "../Results/SAMEGN_50-65",full.names = TRUE)
same_rda_list_2 <- list.files(path = "../Results/SAMEGN_50-80",full.names = TRUE)
diff.rda <- diff_rda_list[c(1,12,23,34)]
diff.rda.2 <- diff_rda_list_2[c(1,12,23,34)]
same.rda <- same_rda_list[c(1,12,23,34)]
same.rda.2 <- same_rda_list_2[c(1,12,23,34)]
heterogenous <- c(diff.rda[c(1,2)],diff.rda.2[c(1,2)],same.rda[c(1,2)],same.rda.2[c(1,2)])
homogenous <- c(diff.rda[c(3,4)],diff.rda.2[c(3,4)],same.rda[c(3,4)],same.rda.2[c(3,4)])

heterogenous.fit.list <- list()
for (k in 1:8){
  path <- (paste0(heterogenous[k],'/run_1/Fitness/'))
  for (j in 1:30){
    load(paste0(path,'Simulation',j,'.rda'))
    heterogenous.fit.list[[length(heterogenous.fit.list)+1]] <- fit
  }
  array <- do.call(cbind, heterogenous.fit.list)
  mean.array <- apply(array, MARGIN = 1,mean, na.rm = TRUE)
  pdf(paste0("../Results/Avg_Heterogenous_Plot_of_Simulations.pdf")) #open pdf
  plot(1:length(mean.array),as.numeric(mean.array),type = 'l',xlab = 'Generations', ylab = 'Average Fitness')
  mtext("Average Fitness Progress of Heterogenous Population",side = 3, line = -2, outer = TRUE)
  dev.off()
}
homogenous.fit.list <- list()
for (m in 1:8){
  path <- (paste0(homogenous[m],'/run_1/Fitness/'))
  for (n in 1:30){
    load(paste0(path,'Simulation',n,'.rda'))
    homogenous.fit.list[[length(homogenous.fit.list)+1]] <- fit
  }
  homogenous.array <- do.call(cbind, homogenous.fit.list)
  homogenous.mean.array <- apply(homogenous.array, MARGIN = 1,mean, na.rm = TRUE)
  pdf(paste0("../Results/Avg_Homogenous_Plot_of_Simulations.pdf")) #open pdf
  plot(1:length(homogenous.mean.array),as.numeric(homogenous.mean.array),type = 'l',xlab = 'Generations', ylab = 'Average Fitness')
  mtext("Average Fitness Progress of Homogenous Population",side = 3, line = -2, outer = TRUE)
  dev.off()
}

plot.colour <- rainbow(30,start = 0, end = 0.25)
for (k in 1:8){
  path <- (paste0(heterogenous[k],'/run_1/Fitness/'))
  pdf(paste0("../Results/Heterogenous_Plot_of_Simulations.pdf")) #open pdf
  for (j in 1:30){
    load(paste0(path,'Simulation',j,'.rda'))
    if (j == 1){
      plot(1:length(fit[,1]),fit[,1],type = 'l',xlab = 'Generations', ylab = 'Fitness',lty=j,col=plot.colour[j])
      mtext("Fitness of Heterogenous Population",side = 3, line = -2, outer = TRUE)
    }else{
      lines(1:length(fit[,1]),fit[,1],type='l',lty=j,col=plot.colour[j])
    }
  }
  dev.off()
}

plot.colour <- rainbow(30,start = 0.7, end = 0.9)
for (k in 1:8){
  path <- (paste0(homogenous[k],'/run_1/Fitness/'))
  pdf(paste0("../Results/Homogenous_Plot_of_Simulations.pdf")) #open pdf
  for (j in 1:30){
    load(paste0(path,'Simulation',j,'.rda'))
    if (j == 1){
      plot(1:length(fit[,1]),fit[,1],type = 'l',xlab = 'Generations', ylab = 'Fitness',lty=j,col=plot.colour[j])
      mtext("Fitness of Homogenous Population",side = 3, line = -2, outer = TRUE)
    }else{
      lines(1:length(fit[,1]),fit[,1],type='l',lty=j,col=plot.colour[j])
    }
  }
  dev.off()
}


                