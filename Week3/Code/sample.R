######### Different Functions and Run Time ##########

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'


myexperiment <- function(popn, n){
  # A function to take a sample of size n from a population "popn" and return its mean
  pop_sample <- sample(popn, n, replace=TRUE)
  return(mean(pop_sample))
}


loopy_sample1 <- function(popn, n, num){
  # Calculate means using a for loop without preallocation
  result1 <- vector() # Initialise empty vector of size 1
  for (i in 1:num){
    result1 <- c(result1, myexperiment(popn, n))
  }
  return(result1)
}

loopy_sample2 <- function(popn, n, num){
  # To run "num" iterations of the experiment using a for loop on a vector with preallocation
  result2 <- vector(,num) #Preallocate expected size
  for (i in 1:num){
    result2[i] <- myexperiment(popn, n)
  }
  return(result2)
}


loopy_sample3 <- function(popn, n, num){
  # To run "num" iterations of the experiment using a for loop on a list with preallocation
  result3 <- vector("list", num)
  for (i in 1:num){
    result3[[i]] <- myexperiment(popn, n)
  }
  return(result3)
}


lapply_sample <- function(popn, n, num){
  # To run "num" iterations of the experiment using vectorization with lapply
  result4 <- lapply(1:num, function(i) myexperiment(popn, n))
  return(result4)
}


sapply_sample <- function(popn, n, num){
  # To run "num" iterations of the experiment using vectorization with lapply
  result5 <- sapply(1:num, function(i) myexperiment(popn, n))
  return(result5)
}

popn <- rnorm(1000)
hist(popn)
n <- 20 # sample size for each experiment
num <- 1000 # Number of times to rerun the experiment

print("The loopy, non-preallocation approach takes:" )
print(system.time(loopy_sample1(popn, n, num)))

print("The loopy, but with preallocation approach takes:" )
print(system.time(loopy_sample2(popn, n, num)))

print("The loopy, non-preallocation approach takes:" )
print(system.time(loopy_sample3(popn, n, num)))

print("The vectorized sapply approach takes:" )
print(system.time(sapply_sample(popn, n, num)))

print("The vectorized lapply approach takes:" )
print(system.time(lapply_sample(popn, n, num)))
