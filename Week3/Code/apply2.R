##constructed functions used for vectorization

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

SomeOperation <- function(v){ # What does this function do
  #multiplies each value in the matrix by 100 if the sum is greater than 0
  if (sum(v) > 0){
    return(v * 100)
  }
  return (v)
}

M <- matrix(rnorm(100), 10, 10)
print(apply(M, 1, SomeOperation))