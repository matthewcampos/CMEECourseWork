## Build a random matrix
M <- matrix(rnorm(100), 10, 10)

## Take the mean of each row
RowMeans <- apply(M, 1, mean) #margin indication: 1=row, 2=col, c(1,2)= row&col and FUN=mean which is the function to be applied
print(RowMeans)

## Now the variance
RowVars <- apply(M, 1, var)
print(RowVars)

## By column
ColMeans <- apply(M, 2, mean)
print(ColMeans)