##Migration plots of averages of the 44 conditions

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

rm(list=ls()) #clear workspace

#open pathway to data
gndiff.50.65.rda_list <- list.files(path = "../Data/GNDIFF_50-65",full.names = TRUE)
gndiff.50.80.rda_list <- list.files(path = "../Data/GNDIFF_50-80",full.names = TRUE)
gnsame.50.65.rda_list <- list.files(path = "../Data/GNSAME_50-65",full.names = TRUE)
gnsame.50.80.rda_list <- list.files(path = "../Data/GNSAME_50-80",full.names = TRUE)
#list of pathways
folder.list <- list(gndiff.50.65.rda_list,gndiff.50.80.rda_list,gnsame.50.65.rda_list,gnsame.50.80.rda_list)
#create directory for plots
dir.create(paste0('../Results/Migration_Fitness_Plots'), recursive = TRUE) 
names <- c("GNDIFF_50-65","GNDIFF_50-80","GNSAME_50-65","GNSAME_50-80")
#Titles for legend
condition <- c('No Mig.', 'Rand Mig.', 'Mig. 1% q1Gen.', 'Mig. 3% q1Gen.', 'Mig. 5% q1Gen.','Mig. 1% q5Gen.',
               'Mig. 1% q10Gen.','Mig. 3% q5Gen.','Mig. 5% q5Gen.','Mig. 3% q10Gen.',
               'Mig. 5% q10Gen.') #q for every
gene.condition <- rep(condition, times=4) #replicates each condition for the 4 genetic makeups
gene_title <- c("H-H","H-h","h-H","h-h") #H for heterogenous h for homogenous
rep.gene.title <- rep(gene_title,each=length(condition)) #replicates each genetic makeup for the 44 conditions
legend.title <- paste(rep.gene.title, gene.condition) #44 condition titles
#Fitness plots- average of each 44 conditions
plot.colour <- list(rainbow(44, start = 0, end = 0.25),rainbow(44, start = 0.26, end = 0.50),
                 rainbow(44, start = 0.51, end = 0.75),rainbow(44, start = 0.76, end = 0.99))#colour for each line
plot.title.names <- c("Different Migrant Genetic Network with Environmetal Distance of 15",
                 "Different Migrant Genetic Network with Environmetal Distance of 30",
                 "Same Migrant Genetic Network with Environmetal Distance of 15",
                 "Same Migrant Genetic Network with Environmetal Distance of 30") #title for plots
check <- 0
check.2<-0
check.3 <-0 
for (l in 1:4){
  fit_plot <- dir.create(paste0('../Results/Migration_Fitness_Plots/',names[l]), recursive = TRUE) #create sub-directories
  check = check + 1
  print(check)
  count <- 0
  for (i in 1:44){
    count <- count + 1
    fit.name <- paste0('fit.list.',count) #creates fitness list for each conditions
    assign(fit.name,list()) #assigns each variable name a list
  }
  pdf(paste0('../Results/Migration_Fitness_Plots/',names[l],"/Plot_of_Simulations.pdf")) #open pdf
  for (k in 1:44){
    check.2 <- check.2 + 1
    print(check.2)
    path <- (paste0(folder.list[[l]][k],'/run_1/Fitness/'))
    name.fit <- paste0('fit.list.',k)
    for (j in 1:15){
      check.3 <- check.3 + 1
      print(check.3)
      load(paste0(path,'Simulation',j,'.rda')) #loads simulations
      name.fit[[j]] <- fit #adds 15 fit runs per conditions to each list
    }
    array <- do.call(cbind, name.fit) #converts to an array
    mean.array <- apply(array, MARGIN = 1,mean, na.rm = TRUE) #gets average so one run per 44 conditions
    print('complete')
    if (k == 1){
      plot(1:length(mean.array),as.numeric(mean.array),type = 'l',xlab = 'Generations', ylab = 'Average Fitness',lty=1) #creates plot of first iteration
    }
    lines(1:length(mean.array),as.numeric(mean.array),type='l',lty=k,col=plot.colour[[l]][k]) #adds other 43 runs
    mtext(plot.title.names[l],side = 3, line = -2, outer = TRUE)
    legend("bottomright", legend = legend.title,col=plot.colour[[l]], lty=1:44, cex=0.4)
  }
  dev.off()
}






