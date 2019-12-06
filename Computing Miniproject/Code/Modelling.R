rm(list=ls())
#notes
#getting NA values for slope
#can lose all data points so end if only 2 remain
#for data sets with negative abundance due to log, used absolute value to deduct from max
get_easy_param_values <- function(data){
  K <- max(data[,3])
  N0 <- min(data[,3])
  return(c(K,N0))
}

get_hard_param_values <- function(data){
  data[,2] <- rev(data[,2])
  data[,3] <- rev(data[,3])
  plot(data[,2],data[,3])
  for (reps in 1:1000){
    n <- lm(data[,3]~data[,2])
    slope <- coef(n)[2]
    max <- max(data[,3])
    min <- min(data[,3])
    n_max <- max - (abs(max) * 0.10) #removes 10% from the max
    n_min <- min * 1.10 #removes 10% from the min
    index_rm <- c()
    for (i in 1:length(data[,3])){
      if (data[i,3] >= n_max || data[i,3] < n_min){
        index_rm <- c(index_rm,i) #saves index where values are within this range
        #print(index_rm)
      }
    }
    data <- data[-index_rm,]
    dim(data)
    if (dim(data)[1] <= 2){
      break
    }
    n <- lm(data[,3]~data[,2])
    #abline(n,col="red")
    #print(n)
    s <- coef(n)[2]
    if (is.na(s)==TRUE){
      s <- 0
    }
    if (s > slope){
      slope <- s #replaces value of slope if steeper
      next
    }
    else if (s < slope){
      #print(coef(n)[2]) #breaks slope once highest slope value is reached (r)
      break
    }
  }
  r <- slope
  #print(r)
  y_int <- coef(n)[1]
  #abline(n, col="red")
  #print(n)
  #print(y_int)
  t_lag <- (-1 * y_int) / r #assume x_intercept to be tlag
  if (t_lag < 0){
    t_lag <- 0
  }
  return(c(r, t_lag))
}

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

csv_list <- list.files(path = "../Results/CSV",full.names = TRUE)
length(csv_list)
k_vector <- c()
N0_vector <- c()
r_vector <- c()
t_lag_vector <- c()
count <- 0
#saves all parameters in a vector
for (len in 1:length(csv_list)){
  count <- count + 1
  print(count)
  d<- read.csv(csv_list[len])
  res <- get_easy_param_values(d)
  other_res <- get_hard_param_values(d)
  k_vector <- c(res[1],k_vector)
  N0_vector <- c(res[2],N0_vector)
  r_vector <- c(other_res[1],r_vector)
  t_lag_vector <- c(other_res[2],t_lag_vector)
}



