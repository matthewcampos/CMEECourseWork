################################################################
################## Wrangling the Pound Hill Dataset ############
################################################################

############# Load the dataset ###############
# header = false because the raw data don't have real headers
MyData <- as.matrix(read.csv("../Data/PoundHillData.csv", header=F, stringsAsFactors=F))

# header = true because we do have metadata headers
MyMetaData <- read.csv("../Data/PoundHillMetaData.csv", header=T, sep=";", stringsAsFactors=F)

############# Inspect the dataset ###############
head(MyData)
dim(MyData)
str(MyData)
fix(MyData)
fix(MyMetaData)

############# Transpose ###############
# To get those species into columns and treatments into rows 
MyData <- t(MyData)
head(MyData)
dim(MyData)

############# Replace species absences with zeros ###############
MyData[MyData == ""] =0

############# Convert raw matrix to data frame ###############
TempData <- as.data.frame(MyData[-1,], stringAsFactors=F) #important as it removes factors and deletes colunm names
colnames(TempData) <- MyData[1,] #assign column names from original data

############# Convert from wide to long format  ###############
require(reshape2) #load reshape2 package
#melt converts wide data format to reduced long by stacking set of columns into a single one 
MyWrangledData <- melt(TempData, id=c("Cultivation", "Block", "Plot", "Quadrat"), variable.name = "Species", value.name = "Count")
MyWrangledData[, "Cultivation"] <- as.factor(MyWrangledData[, "Cultivation"])
MyWrangledData[, "Block"] <- as.factor(MyWrangledData[, "Block"])
MyWrangledData[, "Plot"] <- as.factor(MyWrangledData[, "Plot"])
MyWrangledData[, "Quadrat"] <- as.factor(MyWrangledData[, "Quadrat"])
MyWrangledData[, "Count"] <- as.integer(MyWrangledData[, "Count"])

#as.factor coerces the column to become a factor 
#levels is based on the different types of data i.e. cultivation is months thus 12 levels

str(MyWrangledData)
head(MyWrangledData)
dim(MyWrangledData)

############# Exploring the data (extend the script below)  ###############
require(dplyr)
dplyr::glimpse(MyWrangledData)
dplyr::filter(MyWrangledData, Count > 100) #like subset(), but nicer!
dplyr::slice(MyWrangledData, 10:15) # Look at an arbitrary set of data rows
library(dplyr)
