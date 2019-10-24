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

############# Import necessary packages ###############
require(dplyr)
require(tidyr)

############# Converts data to table class ###############
MyData <- dplyr::tbl_df(MyData) #makes it into a tbl class so easier to examine
dim(MyData)

############# Replace species absences with zeros ###############
MyData[MyData == ""]=0

############# Convert raw matrix to data frame ###############
TempData <- as.data.frame(MyData[-1,], stringsAsFactors = F)
colnames(TempData) <- MyData[1,] #assign column names from original data

############# Convert from wide to long format  ###############
#works better on df and gather condenses the columns while mutate sets the factors and data types
MyWrangleData <- TempData %>% gather(Species, Count, -Cultivation, -Block, -Plot, -Quadrat) %>%  
mutate( Cultivation <- as.factor(Cultivation),
        Block <- as.factor(Block),
        Plot <- as.factor(Plot),
        Quadrat <- as.factor(Quadrat),
        Species <- as.factor(Species),
        Count <- as.integer(Count))

dim(MyWrangleData)
str(MyWrangleData)
