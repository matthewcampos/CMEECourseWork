rm(list=ls())
#notes
#getting NA values for slope
#can lose all data points so end if only 2 remain
#for data sets with negative abundance due to log, used absolute value to deduct from max
#K was calculated as the max of the data
#N0 as the minimum
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
LGrowthData <- read.csv("../Data/LogisticGrowthData.csv", header = TRUE)
species <- unique(LGrowthData[ ,"Species"]) #find unique species
med <- unique(LGrowthData$Medium)
length(med)
temp <- unique(LGrowthData$Temp)
length(temp)

#shift negative values for modelling
min(LGrowthData$PopBio) #find smallest value  which is -668.2839
which(LGrowthData$PopBio==min(LGrowthData$PopBio)) #index 1931 
LGrowthData <- LGrowthData[-1931,] #remove smallest as it is too large
second_min<-min(LGrowthData$PopBio) #find second smallest which is -0.003434656
second_min<-abs(second_min) #make smallest absolute
#first index pop bio 0.2832757 + x = 0.2867104
LGrowthData$PopBio <- LGrowthData$PopBio + second_min #shift by the absolute value of x
min(LGrowthData$PopBio) #find new smallest which is 0
which(LGrowthData$PopBio==0) #index 588
dim(LGrowthData) #4386 10
LGrowthData <- LGrowthData[-588,] #remove 0
dim(LGrowthData) #4385 10 so minimum of 0 removed
min(LGrowthData$PopBio) #0.000144189

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

#save datapoints in a rda file
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
#saves all parameters in a vector and checks that there are 285 values 
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

#Models 
#Logistic Model
Logistic_model <- function(N0,k,r,t){
  return(y <- (N0 * k * exp(r*t))/(k + N0 * (exp(r*t)-1)))
}

#Baryani Model
Baryani_model <- function(tlag,N0,k,r,t){
  h0 <- tlag * r
  return(y <- N0 + (r * t) + ((1/r) * log(exp(-r*t)+exp(-h0)-exp((-r*t)-h0))) - 
           log(1 + (exp(r*t)+(1/r) * log(exp(-r*t)+exp(-h0)-exp((-r*t)-h0))-1)/(exp(k-N0))))
}

#Gompertz Model
Gompertz_model <- function(tlag,N0,k,r,t){
  A <- log(k/N0)
  return(y <- A * exp(-exp(((r * exp(1) * (tlag - t))/A) + 1)))
}

#Buchanan Model
Buchanan_model <- function(tlag,N0,k,r,t){
  return(N0 + (t >= tlag) * (t <= (tlag + (k - N0) * log(10)/r)) * 
           r * (t - tlag)/log(10) + (t >= tlag) * 
           (t > (tlag + (k - N0) * log(10)/r)) * (k - N0))
}

#Quadratic Models
Quadratic_model_2nd_deg <- function(A,B,C,t){
  return(y <- A + (B * t) + (C * t^2))
}

Quadratic_model_3rd_deg <- function(A,B,C,D,t){
  return(y <- A + (B * t) + (C * t^2) + (D * t^3))
}

Quadratic_model_4th_deg <- function(A,B,C,D,E,t){
  return(y <- A + (B * t) + (C * t^2) + (D * t^3) + (E * t^4))
}

#fit models
require(minpack.lm)
csv_list <- list.files(path = "../Results/CSV",full.names = TRUE)
e <- 0
for (mod in 1:length(N0_vector)){
  e <- e + 1
  print(e)
  csv <- read.csv(csv_list[mod])
  csv[ ,2] <- rev(csv[ ,2])
  csv[ ,3] <- rev(csv[ ,3])
  name <- paste0("logistic_fit",mod)
  name <- nlsLM(csv[3] ~ Logistic_model(N0, k, r, t), csv)
  #fit_logistic_ <- nlsLM()
}


