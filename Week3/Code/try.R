## run a simulation that involves sampling from a population with try

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

x <- rnorm(50) #Generate your population
doit <- function(x){
  #produces a random sample of numbers and calculates mean if more than 30 random numbers produced
  x <- sample(x, replace = TRUE) #takes sample of specified size using replacement so numbers can be repeated
  if(length(unique(x)) > 30) {#only take mean if sample was sufficient (unique removes duplicates)
    print(paste("Mean of this sample was:", as.character(mean(x))))
  } 
  else {
    stop("Couldn't calculate mean: too few unique points!")
  }
}

## Try using "try" with vectorization:
result <- lapply(1:100, function(i) try(doit(x), FALSE))

## Or using a for loop:
result <- vector("list", 100) #Preallocate/Initialize
for(i in 1:100) {
  result[[i]] <- try(doit(x), FALSE)
}