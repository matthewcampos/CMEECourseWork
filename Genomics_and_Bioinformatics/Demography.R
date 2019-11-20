rm(list=ls())
myData <- as.matrix(read.csv("genomics_and_bioinformatics/Practicals/turtle.csv", header=FALSE, stringsAsFactors = FALSE, colClasses = c("numeric")))
myData[1:10,1:10]

location_A <- matrix(0, nrow = 20, ncol = 2000, byrow = TRUE)
location_A <- myData[1:20,1:2000] #10 individuals in first location

location_B <- matrix(0, nrow = 20, ncol = 2000, byrow = TRUE)
location_B <- myData[21:40,1:2000] #10 individuals in second location

location_C <- matrix(0, nrow = 20, ncol = 2000, byrow = TRUE)
location_C <- myData[41:60,1:2000] #10 individuals in third location

location_D <- matrix(0, nrow = 20, ncol = 2000, byrow = TRUE)
location_D <- myData[61:80,1:2000] #10 individuals in fourth location

fst <- function(data1, data2){
  
  # calculate frequencies
  fA_1 <- as.numeric(apply(FUN=sum, X=data1, MAR=2)/nrow(data1))
  fA_2 <- as.numeric(apply(FUN=sum, X=data2, MAR=2)/nrow(data2))

  # calculate Ht
  HT <- 2*((fA_1+fA_2)/2)*(1-((fA_1+fA_2)/2))
  
  # calculate Hs
  HS <- (fA_1*(1-fA_1)) + (fA_2*(1-fA_2))
  
  # calculate FST
  FST <- (HT-HS)/HT
  
  # average FST
  FST <- FST[!is.na(FST)] #removes NA
  avg_fst <- mean(FST)
  
  return(avg_fst)
}
AvB <- fst(location_A,location_B)
print(AvB)
AvC <- fst(location_A,location_C)
print(AvC)
AvD <- fst(location_A,location_D)
print(AvD)
BvC <- fst(location_B,location_C)
print(BvC)
BvD <- fst(location_B,location_D)
print(BvD)
CvD <- fst(location_C,location_D)
print(CvD)
y <- c(AvB,AvC,AvD,BvC,BvD,CvD)

len <- 2000
data <- as.matrix(read.csv("genomics_and_bioinformatics/Practicals/turtle.genotypes.csv", stringsAsFactors=F, header=F, colClasses=rep("numeric", len)))

locations <- rep(c("A","B","C","D"), each=10)
distance <- dist(data)
tree <- hclust(distance)
plot(tree, labels=locations)

ab <- 5
ac <- 7
ad <- 45
bc <- 2
bd <- 40
cd <- 38
x <- c(ab,ac,ad,bc,bd,cd)
plot(x,y, main="Average FST for Location Comparisons")









