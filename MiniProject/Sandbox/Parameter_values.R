#find initial parameters
get_parameters_1 <- function(data){
  require(polynom)
  data[,2] <- rev(data[,2])
  data[,3] <- rev(data[,3])
  k <- max(data[,3])
  N0 <- min(data[,3])
  par(mfrow=c(2,2))
  d <- data
  list <- c()
  for (i in 1:dim(data)[1]){ #rows
    if (data[i,3] >= mean(data[,3]) || data[i,3] <= z * 2.5){
      list <- c(list,i)
    }
  }
  d <- data[-list,]
  linear_model <-lm(d[,3]~d[,2])
  r <- coef(linear_model)[2]
  plot(data[,2],data[,3], main="main")
  abline(linear_model, col="red")
  linear_model_summary<- summary(linear_model)
  q <- polynomial(c(coefficients(linear_model)[1],coefficients(linear_model)[2]))
  q <- solve(q)
  tlag=q-N0
  return(k,N0,r,tlag)
}

#save into a csv table

#Lactobaciulus plantarum MRS 20 example
rm(list=ls())
MyData <- read.csv("../Results/CSV/Lactobaciulus plantarum_MRS_20.csv")
MyData[,3] #Abundance column
#finding tlag
rm(list=ls())
MyData <- read.csv("../Results/CSV/Acinetobacter.clacoaceticus..RDA.R._TSB_15.csv")
MyData$Time <- rev(MyData$Time)
MyData$Abundance <- rev(MyData$Abundance)
Data <- MyData
z <- min(MyData$Abundance)
list <-c()
#using mean
for (i in 1:dim(MyData)[1]){ #rows
  if (MyData[i,3] >= mean(MyData[,3]) || MyData[i,3] <= z * 2.5){
    list <- c(list,i)
  }
}
Data <- MyData[-list,]
plot(MyData[,2],MyData[,3], main="main")
n <-lm(Data[,3]~Data[,2])
abline(lm(Abundance~Time, data=Data), col="red")
b<- summary(lm(Abundance~Time, data=Data))
require(polynom)
z
q <- polynomial(c(coefficients(n)[1],coefficients(n)[2]))
q <- solve(q)
tlag=q-z5

#compare adjacent data points to get r
get_parameters <- function(data){
  require(polynom)
  data[,2] <- rev(data[,2])
  data[,3] <- rev(data[,3])
  k <- max(data[,3])
  N0 <- min(data[,3])
  list_r <- c()
  for (i in 1:(dim(data)[1])){ #rows
    N_max <- data[i+1,3] - data[i,3]  
    dT <- data[i+1,2] - data[i,2] 
    list_r <- c(list_r,dN/dT)
  }
  #final_list_r <- c()
  #for (j in 1:length(list_r)){
    #if (list_r[j] >= 1.6){
      #final_list_r <- c(j,final_list_r)
    #}
  #}
  #list_r <- list_r[-final_list_r]
  print(list_r)
  r <- max(list_r)
  return(c(k,N0,r,tlag))
}
get_parameters(MyData)







           
