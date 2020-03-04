r_parameter_values <- function(data){
  #only cut-off values from the max to estimate slope (growth rate) near inflection point
  df <- data #to not alter original dataset
  plot(data[,2]~data[,1])
  orig_max <- max(df[,2])
  index_max <- c()
  r_vect <- c()
  y_int_vect <- c()
  analysis <- lm(df[,2]~df[,1]) #first linear regression
  output <- summary(analysis) #to easily retrieve coefficients
  slope <- output$coefficients[2,1]
  y_int <- output$coefficients[1,1]
  r_vect <- c(slope,r_vect)
  y_int_vect <-c(y_int,y_int_vect)
  y <- c()
  #remove max 
  while(dim(df)[1]>5){ #arbitrary amount left
    df <- df[-dim(df)[1],] #removes last data set each iteration
    analysis_2 <- lm(df[,2]~df[,1]) #linear regression and with fewer saturation data should mean greater slope
    output_2 <- summary(analysis_2)
    slope_2 <- output_2$coefficients[2,1]
    r_vect<-c(slope_2,r_vect) #stores all slopes in a vector till condition met
    y_int2 <- output_2$coefficients[1,1]
    y_int_vect <- c(y_int2,y_int_vect)
  }
  index <- which(r_vect==max(r_vect)) #retrieves greatest slope value
  y_intercept <- y_int_vect[index] #retrieves y-intercept for that index with highest slope value
  x <- seq(0,50)
  y <- c(max(r_vect)*x+y_int_vect[index],y)
  lines(y~x)
  return(c(max(r_vect),y_intercept))
}

t_lag_parameter_value <- function(data){
  y_intercept <- r_parameter_values(data)[2]
  slope <- r_parameter_values(data)[1]
  x_intercept <- min(data) - y_intercept / slope
  if (x_intercept<0){
    x_intercept <- 0
  }
  return(x_intercept)
}

r_max_value <- function(data){
  df <- data
  rows <- 1
  slope_vect <- c()
  y_int_vect <- c()
  #analysis <- lm(df[(rows:(rows + 4)),2]~df[(rows:(rows + 4)),1]) #starting 5 data points and working up
  analysis <- lm(log(df[(rows:(rows + 1)),2])~df[(rows:(rows + 1)),1]) #starting 5 data points and working up
  output <- summary(analysis)
  slope<- output$coefficients[2,1] #slope
  y_int <- output$coefficients[1,1] #intercept
  rows <- rows + 1
  slope_vect <- c(slope,slope_vect)
  y_int_vect <- c(y_int, y_int_vect)
  while (rows < nrow(df)){
    #analysis <- lm(df[(rows:(rows + 4)),2]~df[(rows:(rows + 4)),1])
    analysis <- lm(log(df[(rows:(rows + 1)),2])~df[(rows:(rows + 1)),1])
    output <- summary(analysis)
    slope_2 <- output$coefficients[2,1] #slope
    y_int2 <- output$coefficients[1,1] #intercept
    slope_vect<-c(slope_2,slope_vect)
    y_int_vect <-c(y_int2,y_int_vect)
    #if (slope_2 > slope){
    #   slope <- slope_2
    #  y_int <- output$coefficients[1,1]
    #}
    rows <- rows + 1
  }
  index<- which(slope_vect==max(slope_vect))
  #plot(data[,2]~data[,1])
  plot(log(data[,2])~data[,1], main=count)
  y<-c()
  x <- data[,1]
  y <- c(max(slope_vect)*x+y_int_vect[index],y)
  lines(y~x,col="red")
  #print(slope_vect)
  #print(y_int_vect)
  return(c(max(slope_vect),y_int_vect[index]))
  #return(c(slope,y_int))
}

t_lag_parameter_value <- function(data){
  y_intercept <- r_max_value(data)[2]
  slope <- r_max_value(data)[1]
  x_intercept <- min(data) - y_intercept / slope
  if (x_intercept<0){
    x_intercept <- 0
  }
  return(x_intercept)
}