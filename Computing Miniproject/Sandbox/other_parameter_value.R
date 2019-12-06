rm(list=ls())
MyData <- read.csv("../Results/CSV/Lactobaciulus plantarum_MRS_10.csv")
get_easy_param_values <- function(data){
  data[,2] <- rev(data[,2])
  data[,3] <- rev(data[,3])
  k <- max(data[,3])
  N0 <- min(data[,3])
  return(c(k,N0))
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
    n <- lm(data[,3]~data[,2])
    s <- coef(n)[2]
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
  #abline(n, col="red")
  print(n)
  y_int <- coef(n)[1]
  print(y_int)
  t_lag <- (-1*coef(n)[1]) / coef(n)[2] #assume x_intercept to be tlag
  if (t_lag < 0){
    t_lag <- 0
  }
  return(c(r, t_lag))
}






l <- c()
c <- 0
for (i in 1:length(v)){
  df <- read.csv(v[i])
  c<- c+1
  l <- c(get_hard_param_values(df),l)
}
print(l)
print(c) #checked for any NA values

get_easy_param_values(read.csv(csv_list[4]))[1] #returns k 
get_easy_param_values(read.csv(csv_list[1]))[2] #returns N0











hard_param_values <- function(data){
  c <-0
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
        print(index_rm)
      }
    }
    data <- data[-index_rm,]
    dim(data)
    if (dim(data)[1] <= 2){
      break
    }
    n <- lm(data[,3]~data[,2])
    abline(n,col="red")
    print(n)
    c <- c+1
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
  print(r)
  y_int <- coef(n)[1]
  #abline(n, col="red")
  print(n)
  print(c)
  #print(y_int)
  t_lag <- (-1 * y_int) / r #assume x_intercept to be tlag
  if (t_lag < 0){
    t_lag <- 0
  }
  return(c(r, t_lag))
}


hard_param_values(read.csv(csv_list[53]))

#getting NA values for slope
#can lose all data points so end if only 2 remain
#for data sets with negative abundance due to log, used absolute value to deduct from max

