##functions to get starting parameters

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

N_max_N0_parameter_values <- function(data){
  #using range to get K and N0
  values <- range(data[,2]) 
  K <- values[2] #carrying capacity is the max
  N0 <- values[1] #starting is min
  return(c(N0,K))
}

r_value <- function(data){
  df <- data
  rows <- 1
  slope_vect <- c()
  r <- (df[rows+1,2] - df[rows,2]) / (df[rows+1,1] - df[rows,1]) #dN/dT
  slope_vect <- c(r,slope_vect)
  while (rows < nrow(df)-1){
    rows <- rows + 1
    r_2 <- (df[rows+1,2] - df[rows,2]) / (df[rows+1,1] - df[rows,1]) #slope between two datapoints
    slope_vect <- c(r_2,slope_vect)
  }
  slope_vect <- na.omit(slope_vect) #remove any NA as that would be the maximum produced
  index<- which(slope_vect==max(slope_vect)) #get the max slope 
  if (slope_vect[index] < 0 ){ #if slope is negative make it NA and prevent model fitting
    r <- NA
  }
  y_int <- df[2,2] - (max(slope_vect)*df[1,2]) #y-intercept used for t_lag
  return(c(max(slope_vect),y_int))
}

t_lag_parameter_value <- function(data){ 
  #intercept of growth rate and starting population value
  x_intercept <- (min(data$Abundance) - r_value(data)[2]) / r_value(data)[1] #rearrange slope equation
  if (x_intercept < 0){
    x_intercept <- 0
  }
  return(x_intercept)
}












  


