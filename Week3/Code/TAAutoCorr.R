##script to determine if there is correlation in weather data

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

load("../Data/KeyWestAnnualMeanTemperature.RData")
MyData <- ats #store data in variable
dim(MyData)

#create column of temp from 1:dim[1]-1
z <- dim(MyData)[1]
Temp1 <- MyData[1:z-1, 2] #will contain 99 values starting from year 1901 to 1999

#create column of temp from 2:dim
Temp2 <- MyData[2:z, 2] #data from 1902 to 2000

#Correlation of original data
temp_cor <- cor(Temp1, Temp2)

#Create correlation results array for the simulation
cor_result <- rep(NA,10000) 

#for loop that runs 10000 simulations and computes correlation
for (i in 1:10000){
  s <- sample(MyData[ ,2], size=dim(MyData[1]))
  Temp1 <- s[1:(length(s)-1)] #will contain sampled values from s, excluding final value
  Temp2 <- s[2:(length(s))] #excludes first value
  cor_result[i] <- cor(Temp1,Temp2)
}

#Computes p-value by comparing simulation results to orginal data correlation
p_value <- length(cor_result[cor_result>temp_cor])/length(cor_result) #counts how many trues and divides by number of simulations
print(p_value)

#Scatter plot showing temprature correlation of original data
x <- Temp1
y <- Temp2
plot(x, y, main="Correlation of Temperatures from Year(t+1) and Year(t)", pch=19, frame=FALSE)
abline

#Distribution curve
hist(cor_result, col="blue", main="Histogram of Temperature Correlation Coefficient", xlab="correlation coefficient values")
legend('topleft', c("10,000 sampled correlation coefficient", "original data correlation"), cex=0.5, fill=c("blue", "black"))
abline(v=temp_cor, col="black", lwd=5)

#random distribution so it is showing the frequency of correlation coefficients by chance
#since the original data correlation is outside the distribution, to the right, it leads to a small p_value
#p_value is less than 0.05 so there is a correlation between successive year temperatures and previous years


