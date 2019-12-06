##understanding how to debug in R

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

Exponential <- function(N0 = 1, r = 1, generations = 10){
  # Runs a simulation of exponential growth
  # Returns a vector of length generations
  
  N <- rep(NA, generations)    # Creates a vector of NA of length generations
  
  N[1] <- N0 #sets the first value as the starting population
  for (t in 2:generations){ #calculates predicted population of every succeeding generation
    N[t] <- N[t-1] * exp(r)
    browser() #interupts execution and allows inspection of environment
  }
  return (N)
}

plot(Exponential(), type="l", main="Exponential growth")