rm(list=ls())
##Wrangling the data and sorting based on headers

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

library(dplyr)
LGrowthData <- read.csv("../Data/LogisticGrowthData.csv", header = TRUE)
#View(LGrowthData[which(LGrowthData$PopBio_units=="OD_595"),])
species <- unique(LGrowthData[ ,"Species"]) #find unique species
med <- unique(LGrowthData$Medium)
temp <- unique(LGrowthData$Temp)
#Remove and shift negative number
index <- which(LGrowthData$PopBio == min(LGrowthData$PopBio))
LGrowthData <- LGrowthData[-index,] 
LGrowthData$PopBio <- LGrowthData$PopBio - min(LGrowthData$PopBio) #shift by 0.0343
new_index <- which(LGrowthData$PopBio == min(LGrowthData$PopBio))
LGrowthData <- LGrowthData[-new_index,]
#Change OD_595 
LGrowthData$PopBio[which(LGrowthData$PopBio_units=="OD_595")]<- LGrowthData$PopBio[which(LGrowthData$PopBio_units=="OD_595")] * 100
#Log transform
LGrowthData$PopBio <- log10(LGrowthData$PopBio) 
#Change negative time values to 0
LGrowthData$Time[which(LGrowthData$Time < 0)] <- 0

#Getting unique ID's by subsetting by Species, Medium, Temp then Citation
pdf(paste("../Results/SpeciesID.pdf")) #open pdf
species <- unique(LGrowthData[ ,"Species"]) #find unique species
for (s in species){ 
  species_data <- filter(LGrowthData, Species==s) #uses dplyr to group by species
  medium <- unique(species_data$Medium) #collects the unique growth mediums
  for (m in medium){
    medium_data <- filter(species_data, Medium==m) #then grouped by growth medium
    temperature <- unique(medium_data$Temp) #find unique temperatures
    for (t in temperature){
      temp_data <- filter(medium_data, Temp==t) #then grouped by temperature
      citation <- unique(temp_data$Citation) #find unique citation
      for (c in citation){
        c_data <- filter(temp_data, Citation==c) #group by citation 
        rep <- unique(c_data$Rep)
        for (r in rep){
          rep_data <-filter(c_data, Rep==r) #group by rep to get overall unique ID
          time <- c()
          abundance <- c()
          time <- c(rep_data$Time, time)
          abundance <-c(rep_data$PopBio, abundance)
          data <- data.frame("Time"=time,"Abundance"=abundance)
          data <- data[order(data$Time),] #makes it descending order
          filename <- paste("../Data/RDA/",s,"_",m,"_",t,"_",r,".rda",sep = "")
          save(data,file = filename)
          plot(rep_data$Time, rep_data$PopBio, xlab="Time", ylab="Abundance", main=paste(s,m,t,r, sep = "\n"))
          mtext(c, side = 3, cex = 0.25) #adds citation at the top
        
      }
      mtext(c_data$PopBio_units, side = 2, line = 2, cex = 0.75) #adds pop_bio units to the elft
    }
    
  }
}
}
dev.off() #close pdf file with all graphs


