#functions for growth
Logistic_model <- function(N0,k,r,t){
  return(y <- (N0 * k * exp(r*t))/(k + N0 * (exp(r*t)-1)))
}
#as r increases,the less time it takes for the population to reach k so goes from s shape to r shape
#as k increases, the more time it takes for the population to reach k so more time before it exponentially grows rapidly

Baryani_model <- function(tlag,N0,k,r,t){
  h0 <- tlag * r
  return(y <- N0 + (r * t) + ((1/r) * log(exp(-r*t)+exp(-h0)-exp((-r*t)-h0))) - 
           log(1 + (exp(r*t)+(1/r) * log(exp(-r*t)+exp(-h0)-exp((-r*t)-h0))-1)/(exp(k-N0))))
}
#increasing h0 causes lag before increase and can cause decrease if greater than 1
#increasing k causes graph to become linear
#increasing r causes it to become r shape faster

Gompertz_model <- function(tlag,N0,k,r,t){
  A <- log(k/N0)
  return(y <- A * exp(-exp(((r * exp(1) * (tlag - t))/A) + 1)))
}
#increasing tlag causes the exponential increase to occur later
#increasing r causes the exponential increase to occur more rapidly
#increasing k causes the exponential increase line to become more linear

Buchanan_model <- function(tlag,N0,k,r,t){
  return(N0 + (t >= tlag) * (t <= (tlag + (k - N0) * log(10)/r)) * 
           r * (t - tlag)/log(10) + (t >= tlag) * 
           (t > (tlag + (k - N0) * log(10)/r)) * (k - N0))
}
t <- c(1:1000)
N0 <- 1
k <- 50
r <- 0.5
tlag <-20
y <- Buchanan_model(tlag,N0,k,r,t)
plot(t,y)
#decreasing k lessens the time taken to reach carrying capacity
#increasing tlag causes the time for linear increase to occur later
#increasing r makes the linear growth steeper
#smaller starting value takes longer to reach carrying capacity

Quadratic_model_2nd_deg <- function(A,B,C,t){
  return(y <- A + (B * t) + (C * t^2))
}

Quadratic_model_3rd_deg <- function(A,B,C,D,t){
  return(y <- A + (B * t) + (C * t^2) + (D * t^3))
}

Quadratic_model_4th_deg <- function(A,B,C,D,E,t){
  return(y <- A + (B * t) + (C * t^2) + (D * t^3) + (E * t^4))
}
#increasing degree makes it steeper
#use lm function and poly with set degrees
