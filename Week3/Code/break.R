i <- 0 #Initialize i
while(i < Inf) {
  if (i == 10) {
    break 
  } # Break out of the while loop! 
  else { 
    cat("i equals " , i , " \n")
    i <- i + 1 # Update i
  }
}

for (i in 1:10) {
  if ((i %% 2) == 0) 
    next # pass to next iteration of loop 
  print(i)
}