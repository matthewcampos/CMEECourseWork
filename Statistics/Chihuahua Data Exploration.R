ChihuahuaData <- read.csv("../../rodents.csv")
length(ChihuahuaData$yr)
require(dplyr)
a <- ChihuahuaData %>% group_by(yr) %>% count(length(species)) 
b <- ChihuahuaData %>% group_by(yr, species) %>% summarise(count=n())
b_1 <- b %>% group_by(yr) %>% summarise(count=n()) #counts number of species each year

c <- ChihuahuaData %>% group_by(yr, precip) %>% sum(count=n())
c_1 <- c %>% group_by(yr) %>% summarise(mean(precip)) #gets average precipitation per year
par(mfrow=c(1,1))
plot(b_1$yr,b_1$count, main="Species Richness per year", xlab="Year", ylab = "Species count per Year")
plot(b_1$yr,c_1$`mean(precip)`, main="Precipitation per year", xlab="Year", ylab="Mean Precipitation per Year")
plot(c_1$`mean(precip)`,b_1$count,main="Precipitation and Species Richness", xlab="Mean Precipitation per Year", ylab = "Species count per Year")

