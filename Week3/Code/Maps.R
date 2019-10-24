print(load("../Data/GPDDFiltered.RData")) #load the data 
MyData <- as.data.frame(gpdd) #convert to a dataframe 

require(maps) #loads the map packages

map(database = "world", fill = TRUE, col = rgb(0.2,0.2,0.5,0.5)) #plot world map

points(x = MyData$long, y = MyData$lat, pch =21)

# Examining the plot, we can see that majority of the data is coming from the United States and
# Western Europe. Within both regions, all of the data in North America came from either the US or Canada,
# along the western coast, while in Europe, majority of data comes from the United Kingdom. The data collected are 
# of different species living. It is important to note that the range of environments and conditions are limited to 
# these regions thus conclusions drawn are focused on these areas, and not on a global scale.