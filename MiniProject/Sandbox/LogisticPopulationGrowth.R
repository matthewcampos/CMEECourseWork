rm(list=ls())
library(dplyr)
library(ggplot2)
LGrowthData <- read.csv("../Data/LogisticGrowthData.csv", header = TRUE)
species <- unique(LGrowthData[ ,"Species"]) #find unique species
med <- unique(LGrowthData$Medium)
length(med)
tem <- unique(LGrowthData$Temp)
length(tem)

#shift negative values for modelling
min(LGrowthData$PopBio) #find smallest value  which is -668.2839
which(LGrowthData$PopBio==min(LGrowthData$PopBio)) #index 1931 
LGrowthData <- LGrowthData[-1931,] #remove smallest as it is too large
x<-min(LGrowthData$PopBio) #find second smallest which is -0.003434656
x<-abs(x) #make smallest absolute
#first index pop bio 0.2832757 + x = 0.2867104
LGrowthData$PopBio <- LGrowthData$PopBio + x #shift by the absolute value of x
min(LGrowthData$PopBio) #find new smallest which is 0
which(LGrowthData$PopBio==0) #index 588
dim(LGrowthData) #4386 10
LGrowthData <- LGrowthData[-588,]
dim(LGrowthData) #4385 10 so minimum of 0 removed
min(LGrowthData$PopBio)

#Getting unique ID's by subsetting by Species, Medium, Temp then Citation
pdf(paste("../Results/SpeciesID.pdf")) #open pdf
for (s in species){ 
  species_data <- filter(LGrowthData, Species==s) #uses dplyr to group by species
  medium <- unique(species_data$Medium) #collects the unique growth mediums
  for (m in medium){
    medium_data <- filter(species_data, Medium==m) #then grouped by growth medium
    temperature <- unique(medium_data$Temp) #find unique temperatures
    for (t in temperature){
      temp_data <- filter(medium_data, Temp==t) #then grouped by temperature
      citation <- unique(temp_data$Citation) #find unique citation
      #par(mfrow=c(2,2))
      for (c in citation){
        c_data <- filter(temp_data, Citation==c) #finally group by citation to get overall unique ID
        plot(c_data$Time, log(c_data$PopBio), xlab="Time", ylab="Abundance", main=paste(s,m,t, sep = "\n"))
        mtext(c, side = 3, cex = 0.25) #adds citation at the top
      }
      mtext(c_data$PopBio_units, side = 2, line = 2, cex = 0.75) #adds pop_bio units to the elft
    }
  }
}
dev.off() #close pdf file with all graphs

#save datapoints in a csv file
for (s in species){ 
  species_data <- filter(LGrowthData, Species==s) #uses dplyr to group by species
  medium <- unique(species_data$Medium) #collects the unique growth mediums
  for (m in medium){
    medium_data <- filter(species_data, Medium==m) #then grouped by growth medium
    temperature <- unique(medium_data$Temp) #find unique temperatures
    for (t in temperature){
      temp_data <- filter(medium_data, Temp==t) #then grouped by temperature
      citation <- unique(temp_data$Citation) #find unique citation
      time <- c()
      abundance <- c()
      for (c in citation){
        c_data <- filter(temp_data, Citation==c)#finally group by citation to get overall unique ID
        time <- c(c_data$Time,time)
        abundance <-c(log(c_data$PopBio),abundance)
        df <- data.frame("Time"=time,"Abundance"=abundance)
        write.csv(df,paste("../Results/CSV/",s,"_",m,"_",t,".csv",sep = ""))
      }
    }
  }
}



  
