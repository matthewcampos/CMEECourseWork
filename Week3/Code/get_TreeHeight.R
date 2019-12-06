# This function calculates heights of trees given distance of each tree 
# from its base and angle to its top, using  the trigonometric formula 
#
# height = distance * tan(radians)
#
# ARGUMENTS
# degrees:   The angle of elevation of tree
# distance:  The distance from base of tree (e.g., meters)
#
# INPUT
# input file entered in command line
# OUTPUT
# The heights of the tree, same units as "distance"

#!/usr/bin/env Rscript

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

args = commandArgs(trailingOnly=TRUE)

#checks if there is an input in the command line
if (length(args)==0){
  stop("At least one argument must be supplied (input file).n. Please check Data directory folder")
}

#sets the file to a variable
MyData <- read.csv(args[1], header=TRUE)
print(MyData)

#figure out how many distance measurements were taken and transfer to a vector
distance_vector <- vector()
for (i in MyData[2]){
  distance_vector <- c(i)
}
print(length(distance_vector)) 

#count angle degrees measurements and put them in a vector
angle_degrees_vector <- vector()
for (y in MyData[3]){
  angle_degrees_vector <- c(y)
}
print(length(angle_degrees_vector)) 

#calculates the tree height
TreeHeight <- function(degrees, distance){ 
  #calculates height using trignometric rules
  radians <- degrees * pi / 180
  height <- distance * tan(radians)
  #print(paste("Tree height is:", height))
  
  return (height)
}

#runs function to calculate tree height and assigns values into vector
Tree_height.m <- vector() #creates empty vector
for (z in 1:120){
  Tree_height.m <- c(Tree_height.m,TreeHeight(angle_degrees_vector[z],distance_vector[z]))
}

#Edit output name
file <- args[1] 
output_name <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(file)) #removes file path and extension
print(output_name)

#creating file output
path_out = "../Results/" #create relative path
filename <- paste(path_out,output_name,"_treeheight.csv ", sep = "")

MyData <- cbind(MyData,Tree_height.m) #adds column of the vector of tree height to 
write.csv(MyData, filename)
