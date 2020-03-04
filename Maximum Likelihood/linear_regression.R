recapture.data <- read.csv('recapture.csv',header = T)
plot(recapture.data$day, recapture.data$length_diff, pch=19)
regression.log.likelihood <- function(param, dat){
  #define parameters
  a = param[1]
  b = param[2]
  sigma = param[3]
  
  #define the data
  x <- dat[,1] #explanatory
  y <- dat[,2] #response
  
  #define error term
  error.term <- (y - a - b * x)
  
  #normal pdf
  density <- dnorm(error.term, mean=0, sd=sigma, log = T)
  
  #log likelihood is the sum of the individual log density
  return(sum(density))
}
#check for errors
regression.log.likelihood(c(1,1,1), dat = recapture.data)

#using optim to maximise
#control = list(fnscale=-1) means to maximise
#L-BFGS-B is optimisation algorithm
#hessian matrix provide information about the variance-covariance structure of your parameter estimates
#make sure message is converge
optim(par = c(1,1,1), regression.log.likelihood, method = "L-BFGS-B", lower = c(-1000,-1000,0.0001), upper = c(1000,1000,10000),
      control = list(fnscale=-1), dat= recapture.data, hessian=T)

# THE LOG-LIKELIHOOD FUNCTION FOR M1 WITHOUT AN INTERCEPT
regression.no.intercept.log.likelihood<-function(parm, dat)
{
  # DEFINE THE PARAMETERS
  # NO INTERCEPT THIS TIME
  # NO INTERCEPT THIS TIME
  b<-parm[1]
  sigma<-parm[2]
  # DEFINE THE DATA
  # SAME AS BEFORE
  x<-dat[,1]
  y<-dat[,2]
  # DEFINE THE ERROR TERM, NO INTERCEPT HERE
  error.term<-(y-b*x)
  # REMEMBER THE NORMAL pdf?
  density<-dnorm(error.term, mean=0, sd=sigma, log=T)
  #LOG-LIKELIHOOD IS THE SUM OF DENSITIES
  return(sum(density))
}

M1<-optim(par=c(1,1), regression.no.intercept.log.likelihood,
          dat=recapture.data, method='L-BFGS-B',
          lower=c(-1000,0.0001), upper=c(1000,10000),
          control=list(fnscale=-1), hessian=T)
M2<-optim(par=c(1,1,1), regression.log.likelihood,
          dat=recapture.data, method='L-BFGS-B',
          lower=c(-1000,-1000,0.0001), upper=c(1000,1000,10000),
          control=list(fnscale=-1), hessian=T)
# THE TEST STATISTIC D
D<-2*(M2$value-M1$value)
D
#CRITICAL VALUE
qchisq(0.95, df=1)

flowering <- read.table('flowering.txt', header=T)
names(flowering)
par(mfrow=c(1,2))
plot(flowering$Flowers, flowering$State)
plot (flowering$Root, flowering $State)

#two arguments: parm is a vector of paremeters,
#dat is the input dataset
logistic.log.likelihood <- function(parm, dat){
  #define parameters
  a <- parm[1]
  b <- parm[2]
  c <- parm[3]
  #define response variable
  State <- dat[,1]
  #define explanatory variable
  Flowers <- dat[,2]
  Root <- dat[,3]
  #model success probability
  p <- exp(a+b*Flowers+c*Root) / (1+exp(a+b*Flowers+c*Root))
  #Log-likelihood function
  log.like <- sum(State*log(p)+(1-State)*log(1-p))
  return(log.like)
}
logistic.log.likelihood(c(0,0,0), dat=flowering)
M1 <- optim(par = c(0,0,0),logistic.log.likelihood, dat=flowering, control = list(fnscale=-1))

logistic.log.likelihood.int <- function(parm, dat){
  #define parameters
  a <- parm[1]
  b <- parm[2]
  c <- parm[3]
  d <- parm[4]
  #define response variable
  State <- dat[,1]
  #define explanatory variable
  Flowers <- dat[,2]
  Root <- dat[,3]
  interaction <- dat[,2]*dat[,3]
  #model success probability
  p <- exp(a+b*Flowers+c*Root+d*interaction) / (1+exp(a+b*Flowers+c*Root+d*interaction))
  #Log-likelihood function
  log.like <- sum(State*log(p)+(1-State)*log(1-p))
  return(log.like)
}
M2 <- optim(par = c(0,0,0,0),logistic.log.likelihood.int, dat=flowering, control = list(fnscale=-1))

D <- 2*(M2$value-M1$value)
D #cannot be negative
qchisq(0.95, df=1) #compare d value with chi-square

# DEFINE THE RANGE OF PARAMETERS TO BE PLOTTED
b<-seq(2, 4, 0.1)
sigma<-seq(2, 5, 0.1)
# THE LOG-LIKELIHOOD VALUE IS STORED IN A MATRIX
log.likelihood.value<-matrix(nr=length(b), nc=length(sigma))
# COMPUTE THE LOG-LIKELIHOOD VALUE FOR EACH PAIR OF PARAMETERS
for (i in 1:length(b))
{
  for (j in 1:length(sigma))
  {
    log.likelihood.value[i,j]<-
      regression.no.intercept.log.likelihood(parm=c(b[i],sigma[j]),
                                             dat=recapture.data)
  }
}
# WE ARE INTERESTED IN KNOWING THE RELATIVE LOG-LIKELIHOOD VALUE
# RELATIVE TO THE PEAK (MAXIMUM)
rel.log.likelihood.value<-log.likelihood.value-M1$value
# FUNCTION FOR 3D PLOT
persp(b, sigma, rel.log.likelihood.value, theta=30, phi=20,
      xlab='b', ylab='sigma', zlab='rel.log.likelihood.value',
col='grey')

# CONTOUR PLOT
contour(b, sigma, relative.log.likelihood.value, xlab='b',
        ylab= 'sigma',
        xlim=c(2.5, 3.9), ylim=c(2.0, 4.3),
        levels=c(-1:-5, -10), cex=2)
# DRAW A CROSS TO INDICATE THE MAXIMUM
points(M1$par[1], M1$par[2], pch=3)
#We can, again, draw the -1.92 line (circle) on the contour map
contour.line<-contourLines(b, sigma, rel.log.likelihood.value, levels=-1.92)[[1]]
lines(contour.line$x, contour.line$y, col='red',
      lty=2, lwd=2)










