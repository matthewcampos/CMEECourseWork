##using break to stop loops

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

i <- 0 #Initialize i
while(i < Inf) { #i is less than infinite
  if (i == 10) { #i equals 10
    break 
  } # Break out of the while loop! 
  else { 
    cat("i equals " , i , " \n") #concatenate and print i equals followed by value
    i <- i + 1 # Update i by incrementing it
  }
}

for (i in 1:10) { #i ranges from 1:10
  if ((i %% 2) == 0) #i mod 2 and remainder is 0
    next # pass to next iteration of loop 
  print(i)
}