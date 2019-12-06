##practice with if, for and while loops in R

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

## IF Statement
a <- TRUE
if (a==FALSE){
  print("a is True")
  } else {
  print("a is False")
  }

## IF Statement on a single line
z <- runif(1) ## uniformly distributed number
if (z <= 0.5) {print("Less than half")}

## For loop using a sequence
for (i in 1:10){ #i is a range from 1 to 10
  j <- i * i #j is i squared
  print(paste(i, " squared is", j ))
}

## For loop over vector of strings
for(species in c('Heliodoxa rubinoides', 
                 'Boissonneaua jardini', 
                 'Sula nebouxii')){
  print(paste('The species is', species)) #species takes on the value in the vector
}

## for loop using a vector
v1 <- c("a","bc","def")
for (i in v1){ #takes on the values in vector
  print(i)
}

## While loop
i <- 0
while (i<10){ #breaks when i reaches 10
  i <- i+1 #increments i each iteration
  print(i^2) #prints the square of i
}





