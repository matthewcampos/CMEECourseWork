rm(list=ls())

2*2+1
2*(2+1)
[1]
12/2^3
(12/2)^3
x <- 5
y<- 2
x2 <- x^2
x2
x
a <- x2+x
a
y2 <- y^2
z2 <- x2+y2
z <- sqrt(z2)
print(z)
3>2
3>=3
4<2
myNumericVector <- c(1.3,2.5,1.9,3.4,5.6,1.4,3.1,2.9)
myCharacterVector <- c("low", "low", "low", "low", "high", "high", "high", "high")
myLogicalVector <- c(TRUE,TRUE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE)
str(myNumericVector)
str(myCharacterVector)
str(myLogicalVector)
myMixedVector <- c(1,TRUE,FALSE,3,"help",1.2,TRUE,"notwhatIplanned")
str(myMixedVector)

library(lme4)
require(lme4)
help(getwd)
help(log)
sqrt(4);4^0.5;log(0);log(1);log(10);log(Inf)

d <- read.table("SparrowSize.txt", header=TRUE)
d
str(d)
head(d)

