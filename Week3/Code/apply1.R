##collection of functions that vectorize code for you

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

## Build a random matrix
M <- matrix(rnorm(100), 10, 10) #produces a 10x10 matrix of 100 random numbers from normal distribution

## Take the mean of each row
RowMeans <- apply(M, 1, mean) #margin indication: 1=row, 2=col, c(1,2)= row&col and FUN=mean which is the function to be applied
print(RowMeans)

## Now the variance
RowVars <- apply(M, 1, var) #calculates variance per row
print(RowVars)

## By column
ColMeans <- apply(M, 2, mean) #calculates mean of each column in the matrix
print(ColMeans)