source("functions.R")
#function L times pi 
#parameter space
x <- seq(0,1,0.01)

#g is a uniform envelope function

#M is a set value 
M <- 250000

#initialise vector that contains accepted values
thetas <- c()

#we want N Samples
N <- 1e4

while (length(thetas) < N){
  g_theta <- runif(1,0,1)
  U <- runif(1,0,1)
  result <- M * U
  if (result < L_times_pi(g_theta)){
    thetas <- c(result,thetas)
  }
}

hist(thetas)

## check with a qq-plot
#truePosterior <- rbeta(N, 3, 10)
#qqplot(...)
#abline(0,1)
