rm(list=ls())
d <- read.table("SparrowSize.txt", header=TRUE)
str(d)
names(d)
length(d$Tarsus)
hist(d$Tarsus)
mean(d$Tarsus)
mean(d$Mass, na.rm=TRUE)
help(mean)
median(d$Tarsus, na.rm=TRUE)
mode(d$Tarsus)
par(mfrow=c(2,2)) #shows multiple plots in output
hist(d$Tarsus, breaks=3, col="grey")
hist(d$Tarsus, breaks=10, col="grey")
hist(d$Tarsus, breaks=30, col="grey")
hist(d$Tarsus, breaks=100, col="grey")
require(modeest)
mlv(d$Tarsus)
d2 <- subset(d, d$Tarsus!="NA")

#Bill length
bill.length <- subset(d,d$Bill!="NA")
x <- mean(bill.length$Bill)
var<- sum((bill.length$Bill-x)^2)/(length(bill.length$Bill)-1)
s.d <- sqrt(var)
hist(bill.length$Bill)

#Body Mass
body.mass <- subset(d,d$Mass!="NA")
y <- mean(body.mass$Mass)
var2 <- sum((body.mass$Mass-y)^2)/(length(body.mass$Mass)-1)
s.d2 <- sqrt(var2)
hist(body.mass$Mass)

#Wing length
wing.length <- subset(d, d$Wing!="NA")
z <- mean(wing.length$Wing)
var3 <- sum((wing.length$Wing-z)^2)/(length(wing.length$Wing)-1)
s.d3 <- sqrt(var3)
hist(wing.length$Wing)




