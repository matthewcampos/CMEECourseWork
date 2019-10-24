x <- 1:20
x
y <- factor(rep(letters[1:5], each=4))
y
tapply(x,y,sum) #sum x's based on the tag y so the first 4 values in x are a, next 4 are b, etc.

attach(iris)
iris
iris[1]
iris$Species
by(iris[1:2], iris$Species, colMeans)
iris$Petal.Width
by(iris[1:2], iris$Petal.Width, colMeans) #choosing the columns to calculate, the characterization, function

random <- replicate(10,runif(5))
random [,1]

doit <- function(x){
  temp_x <- sample(x, replace = TRUE)
  if(length(unique(temp_x)) > 30) {#only take mean if sample was sufficient
    print(paste("Mean of this sample was:", as.character(mean(temp_x))))
  } 
  else {
    stop("Couldn't calculate mean: too few unique values!")
  }
}

x <- rnorm(50) 

lapply(1:15, function(i) doit(popn))

result <- lapply(1:15, function(i) try(doit(x), FALSE))

class(result)

result

result <- vector("list", 15) #Preallocate/Initialize
for(i in 1:15) {
  result[[i]] <- try(doit(x), FALSE)
}




