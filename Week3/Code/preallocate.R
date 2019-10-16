a <- NA
Loop_one <- function(){
  for (i in 1:100) {
    a <- c(a, i)
    #print(a)
    #print(object.size(a))
  }
}
print(system.time(Loop_one())) #slower as it is allocating new memory space for each iteration

a <- rep(NA, 100)
Loop_two <- function(){
  for (i in 1:100) {
    a[i] <- i
    #print(a)
    #print(object.size(a))
  }
}
print(system.time(Loop_two())) #faster as it allocates a set amount of memory space for all iterations
