Ricker <- function(N0=1, r=1, K=10, generations=50)
{
  # Runs a simulation of the Ricker model
  # Returns a vector of length generations
  
  N <- rep(NA, generations)    # Creates a vector of NA
  
  N[1] <- N0
  for (t in 2:generations)
  {
    N[t] <- N[t-1] * exp(r*(1.0-(N[t-1]/K)))
  }
  return (N)
}

plot(Ricker(generations=10), type="l")

print(system.time(Ricker(1,1,10,50)))
#elapsed=0.005
"--------------------------------------------------------------------"
# Runs the stochastic (with gaussian fluctuations) Ricker Eqn .
rm(list=ls())
#Formula Nt+1=Nte^r(1−Ntk)

stochrick<-function(p0=runif(1000,.5,1.5),r=1.2,K=1,sigma=0.2,numyears=100)
{
  #initialize using matrix
  N<-matrix(NA,numyears,length(p0))
  N[1,]<-p0
  
  for (pop in 1:length(p0)) #loop through the populations
  {
    for (yr in 2:numyears) #for each pop, loop through the years
    {
      N[yr,pop]<-N[yr-1,pop]*exp(r*(1-N[yr-1,pop]/K)+rnorm(1,0,sigma))
    }
  }
  return(N)
  
}
print("The time stochrick takes is:")
print(system.time(stochrick(runif(1000,.5,1.5),1.2,1,0.2,100))) #elapsed = 0.261
"--------------------------------------------------------------------"
# Now write another function called stochrickvect that vectorizes the above 
# to the extent possible, with improved performance: 

# print("Vectorized Stochastic Ricker takes:")
# print(system.time(res2<-stochrickvect()))

stochrickvect<-function(p0=runif(1000,0.5,1.5),r=1.2,K=1,sigma=0.2,numyears=100)
{
  #initialize using vector rather than matrix
  N<-matrix(NA,numyears,length(p0))
  N[1,]<-p0 #adds 1000 elements into vector
  
    for (yr in 2:numyears) #removed one for loop, and just use the loop through the years
    {
      N[yr,]<-N[yr-1,]*exp(r*(1-(N[yr-1,]/K))+rnorm(1,0,sigma)) #rnorm is number of observations,mean,s.d that takes into account fluctuations i.e.natural disaster
    }
  return(N)
}
print("Vectorized Stocahstic Ricker takes:")
print(system.time(res2<-stochrickvect())) #elapsed = 0.010







