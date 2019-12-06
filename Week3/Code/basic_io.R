# A simple script to illustrate R input-output.  
# Run line by line and check inputs outputs to understand what is happening 

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

MyData <- read.csv('../Data/trees.csv', header = TRUE) # import with headers
print(MyData)
write.csv(MyData, '../Results/MyData.csv') #write it out as a new file
suppressWarnings(write.table(MyData[1,], file='../Results/MyData.csv', append=TRUE)) # Append to it
write.csv(MyData, "../Results/MyData.csv", row.names=TRUE) # write row names
write.table(MyData, "../Results/MyData.csv", col.names=FALSE) # ignore column names


