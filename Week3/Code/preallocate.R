##understanding how memory allocation works

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

a <- NA #a is set to NA
Loop_one <- function(){
  #adds each value of i from 1 to 100 to a vector a each iteration
  for (i in 1:100) {
    a <- c(a, i)
    #print(a)
    #print(object.size(a))
  }
}
print(system.time(Loop_one())) #slower as it is allocating new memory space for each iteration

a <- rep(NA, 100) #creates NA vector of length 100
Loop_two <- function(){
  #adds i to each position in a vector
  for (i in 1:100) {
    a[i] <- i
    #print(a)
    #print(object.size(a))
  }
}
print(system.time(Loop_two())) #faster as it allocates a set amount of memory space for all iterations
