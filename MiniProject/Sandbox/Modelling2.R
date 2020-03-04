rm(list=ls())
#notes
#getting NA values for slope
#can lose all data points so end if only 2 remain
#for data sets with negative abundance due to log, used absolute value to deduct from max
#K was calculated as the max of the data
#N0 as the minimum
get_easy_param_values <- function(data){
  values <- range(data[,2])
  K <- values[1]
  N0 <- values[2]
  return(c(K,N0))
}

get_hard_param_values <- function(data){
  #plot(data[,1],data[,2])
  for (reps in 1:1000){
    n <- lm(data[,2]~data[,1])
    slope <- coef(n)[2]
    max <- max(data[,2])
    min <- min(data[,2])
    n_max <- max - (max * 0.10) #removes 10% from the max
    n_min <- min * 1.10 #removes 10% from the min
    index_rm <- c()
    for (i in 1:length(data[,2])){
      if (data[i,2] >= n_max || data[i,2] < n_min){
        index_rm <- c(index_rm,i) #saves index where values are within this range
        #print(index_rm)
      }
    }
    data <- data[-index_rm,]
    dim(data)
    if (dim(data)[1] <= 2){
      break
    }
    n <- lm(data[,2]~data[,1])
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
  return(c(as.numeric(r), as.numeric(t_lag)))
}

library(dplyr)
LGrowthData <- read.csv("../Data/LogisticGrowthData.csv", header = TRUE)
View(LGrowthData[which(LGrowthData$PopBio_units=="OD_595"),])
species <- unique(LGrowthData[ ,"Species"]) #find unique species
med <- unique(LGrowthData$Medium)
length(med)
temp <- unique(LGrowthData$Temp)
length(temp)

#shift negative values for modelling
LGrowthData$PopBio[which(LGrowthData$PopBio < 0)] <- 0 #set negatives to 0
#Change OD_595 
LGrowthData$PopBio[which(LGrowthData$PopBio_units=="OD_595")]<- LGrowthData$PopBio[which(LGrowthData$PopBio_units=="OD_595")] * 100
#Log transform
LGrowthData$PopBio <- log(LGrowthData$PopBio + 1) 



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
        data <- data.frame("Time"=time,"Abundance"=abundance)
        data <- data[order(data$Time),] #makes it descending order
        filename <- paste("../Results/RDA/",s,"_",m,"_",t,".rda",sep = "")
        save(data,file = filename)
      }
    }
  }
}

#load rda files
rda_list <- list.files(path = "../Results/RDA",full.names = TRUE)
length(rda_list)
k_vector <- c()
N0_vector <- c()
r_vector <- c()
t_lag_vector <- c()
count <- 0
#saves all parameters in a vector and checks that there are 285 values 
for (len in 1:length(rda_list)){
  count <- count + 1
  print(count)
  load(rda_list[len])
  k_vector <- c(get_easy_param_values(data)[2],k_vector)
  N0_vector <- c(get_easy_param_values(data)[1],N0_vector)
  other_res <- get_hard_param_values(data)
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

#Quadratic Model
Quadratic_model_2nd_deg <- function(A,B,C,t){
  return(y <- A + (B * t) + (C * t^2))
}
#Cubic Model
Cubic_model_3rd_deg <- function(A,B,C,D,t){
  return(y <- A + (B * t) + (C * t^2) + (D * t^3))
}

t <- data[,1]
name <- nlsLM(data[,2] ~ Logistic_model(N0, k, r, t = t), data = data, list(N0 = N0_vector[1], k = k_vector[1], r = r_vector[2]) )





#fit models
require(minpack.lm)
RDA <- list.files(path = "../Results/RDA",full.names = TRUE)
e <- 0
for (mod in 1:length(N0_vector)){
  e <- e + 1
  print(e)
  load(RDA[mod])
  if (df[1,1] > df[2,1]){
    df[,1] <- rev(df[,1])
    df[,2] <- rev(df[,2])
  }
  name <- paste0("logistic_fit",mod)
  name <- nlsLM(df["Abundance"] ~ Logistic_model(N0, k, r, t = df[,1]), data = df, list(N0 = N0_vector[mod], k = k_vector[mod], r = r_vector[mod]) )
  #fit_logistic_ <- nlsLM()
}

