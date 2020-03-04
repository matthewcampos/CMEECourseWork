p <- seq(0.01,0.99,0.01)
log.likelihood <- function(param,y,n){
  #define parameters
  p <- param
  #Likelihood calculatoin
  L_p <- choose(n,y) * p ^ y * (1 - p) ^ (n - y)
  #log likelihood
  l_p <- log(L_p)
  return(l_p)
}
optimize(log.likelihood, interval = c(0,1), n=50,y=35, maximum = T)

plot(log.likelihood(p,35,50)~p, type="l", xlim=c(0.55,0.85), ylim=c(-5,-2))
abline(h = -2.100895 -1.92, col='red', lty=2)
#Lower CI
uniroot(function(p){log.likelihood(p,y=35,n=50)+2.1-1.92}, interval = c(0,0.7))
uniroot(function(p){log.likelihood(p,y=35,n=50)+2.1+1.92}, interval = c(0,0.7))
#Upper CI
uniroot(function(p){log.likelihood(p,y=35,n=50)+2.1+1.92}, interval = c(0.7,1))

M2
#Profile Log-Likelihood For b
profile.log.likelihood<-function(b){
  f <- function(parm_acd)
  {logistic.log.likelihood.int(c(parm_acd[1],b,parm_acd[2],parm_acd[3]), dat=flowering)}
  temp <- optim(c(0,0,0), f, control = list(fnscale=--1))
  return(temp$value)
}
profile.log.likelihood(b=-0.03)

#plot the profile log-likelihood for a range of b, usually around its MLE
b<-seq(-0.19,-0.004,0.002)
profile.log.likelihood.value<-rep(NA,length(b))
for (i in 1:length(b)){
  profile.log.likelihood.value[i] <- profile.log.likelihood(b[i])
}
plot(b,profile.log.likelihood.value,type='l')
#draw a horizontal line which is 1.92 units below the maximum
abline(h=M2$value-1.92, col='red', lty=2)



