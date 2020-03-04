# Open R and load all R functions and data needed:
source("../Math/Data/functions.R")
load("Data/polar.brown.sfs.Rdata")

# You can plot this spectrum:
plot2DSFS(polar.brown.sfs, xlab="Polar", ylab="Brown", main="2D-SFS")

nChroms.polar <- nrow(polar.brown.sfs)-1
nChroms.polar
nChroms.brown <- ncol(polar.brown.sfs)-1
nChroms.brown

nrSites <- sum(polar.brown.sfs, na.rm=T)
nrSites

obsSummaryStats <- calcSummaryStats(polar.brown.sfs)
obsSummaryStats
# These are the OBSERVED summary statistics! Keep them.

nrSimul <- 1000 # but change this accordingly

# first, set the path to the "ms" software you installed
msDir <- "Software/msdir/ms" # this is my specific case, yours could be different

# second, set the name for the output text file
fout <- "ms.txt" # leave it like here
mat <- matrix(NA,nrow = nrSimul, ncol = 10)
nam <- c("fst", "pivar1", "pivar2", "sing1", "sing2", "doub1", "doub2","pef","puf","t")
colnames(mat) <- nam
count = 0
for (i in 1:nrSimul){
  count = count + 1
  print(count)
  t= runif(1,2e5,7e5)
  # then we can simulate data:
  simulate(T=t, M=0, nrSites, msDir, fout)
  # and finally calculate the summary statistics for this simulation 
  #(note that you need to specify the number of chromosomes for the two species)
  simulatedSFS <- fromMStoSFS(fout, nrSites, nChroms.polar, nChroms.brown)
  x=calcSummaryStats(simulatedSFS)
  mat[i,1] <- x[1]
  mat[i,2] <- x[2]
  mat[i,3] <- x[3]
  mat[i,4] <- x[4]
  mat[i,5] <- x[5]
  mat[i,6] <- x[6]
  mat[i,7] <- x[7]
  mat[i,8] <- x[8]
  mat[i,9] <- x[9]
  mat[i,10] <- t
}

#scaling the simulated data
scale_data <- matrix(NA,nrow = nrSimul, ncol = 9)
nams <- c("fst", "pivar1", "pivar2", "sing1", "sing2", "doub1", "doub2","pef","puf")
colnames(scale_data) <- nams
for (k in 1:dim(scale_data)[2]){
  scale_data[,k] <- scale(mat[,k])
}

#checking plots
#plot(mat[,10],scale_data[,1],main = paste(nam[1],cor(mat[,10],mat[,1])))
#plot(mat[,10],scale_data[,2],main = paste(nam[2],cor(mat[,10],mat[,1])))
#plot(mat[,10],scale_data[,3],main = paste(nam[3],cor(mat[,10],mat[,1])))
#plot(mat[,10],scale_data[,4],main = paste(nam[4],cor(mat[,10],mat[,1])))
#plot(mat[,10],scale_data[,5],main = paste(nam[5],cor(mat[,10],mat[,1])))
#plot(mat[,10],scale_data[,6],main = paste(nam[6],cor(mat[,10],mat[,1])))
#plot(mat[,10],scale_data[,7],main = paste(nam[7],cor(mat[,10],mat[,1])))
#plot(mat[,10],scale_data[,8],main = paste(nam[8],cor(mat[,10],mat[,1])))
#plot(mat[,10],scale_data[,9],main = paste(nam[9],cor(mat[,10],mat[,1])))
#keep fst 
# you can even plot the simulated site frequency spectrum
plot2DSFS(simulatedSFS, xlab="Polar", ylab="Brown", main="simulated 2D-SFS")

#plot simulated FST distribution and check where observed FST is 
hist(mat[,1])
abline(v=as.numeric(obsSummaryStats[1]))

library(abc)
#target
target <- obsSummaryStats[1] #FST
#simulated FST data with target to scale
sim_fst <- matrix(NA,ncol = 1,nrow = length(mat[,1])+1)
sim_fst <- c(mat[,1],as.numeric(target))
scale_sim_fst <- scale(sim_fst)
result <- abc(target=scale_sim_fst[101],param=mat[,10],sumstat=scale_sim_fst[1:100], tol = 0.10, method = "rejection")
hist(result)
res_sum <- summary(result)

result




