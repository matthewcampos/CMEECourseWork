# This function calculates heights of trees given distance of each tree 
# from its base and angle to its top, using  the trigonometric formula 
#
# height = distance * tan(radians)
#
# ARGUMENTS
# degrees:   The angle of elevation of tree
# distance:  The distance from base of tree (e.g., meters)
#
# OUTPUT
# The heights of the tree, same units as "distance"

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

MyData <- read.csv('../Data/trees.csv', header = TRUE)
print(MyData)
#figure out how many distnace measurements were taken and transfer to a vector
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

TreeHeight <- function(degrees, distance){ 
  #calculates the tree height
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

MyData <- cbind(MyData,Tree_height.m) #adds column of the vector of tree height to 
write.csv(MyData, "../Results/TreeHts.csv")


