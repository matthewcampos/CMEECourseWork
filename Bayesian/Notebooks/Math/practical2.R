#A
#10 samples
library(coda)
alpha <- 1
beta <- 1
n <- 10
K <- 2
x <- seq(0,1,0.01)

prior <- dbeta(x=x, shape1= alpha, shape2=beta)
par(mfrow=c(1,2))
plot(prior,main="Prior distribution",type="l")
likelihood <- dbinom(x=K, size=n, prob=x)
posterior_prob <- dbeta(x=x, shape1 = alpha + K, shape2= beta + n - K)
MAP=x[which(posterior_prob==max(posterior_prob))]
print(paste("MAP=",MAP))

plot(x,posterior_prob,type="l",main="Posterior Distribution")

a <- rbeta(n=1e6, shape1=alpha + K, shape2=beta + n - K)
hpd <- HPDinterval(as.mcmc(a), prob=0.95)
hpd
abline(v=hpd, lty=2)

quantile(a)

#100 samples
library(coda)
alpha <- 1
beta <- 1
n <- 100
K <- 20
x <- seq(0,1,0.01)

prior <- dbeta(x=x, shape1= alpha, shape2=beta)
par(mfrow=c(1,2))
plot(prior,main="Prior distribution",type="l")
likelihood <- dbinom(x=K, size=n, prob=x)
posterior_prob <- dbeta(x=x, shape1 = alpha + K, shape2= beta + n - K)
MAP=x[which(posterior_prob==max(posterior_prob))]

plot(x,posterior_prob,type="l",main="Posterior Distribution")

a <- rbeta(n=1e6, shape1=alpha + K, shape2=beta + n - K)
hpd <- HPDinterval(as.mcmc(a), prob=0.95)
hpd
abline(v=hpd, lty=2)

print(paste("MAP=",MAP))
quantile(a)

#100 samples
library(coda)
alpha <- 1
beta <- 1
n <- 100
K <- 20
x <- seq(0,1,0.01)

prior <- dbeta(x=x, shape1= alpha, shape2=beta)
par(mfrow=c(1,2))
plot(prior,main="Prior distribution",type="l")
likelihood <- dbinom(x=K, size=n, prob=x)
posterior_prob <- dbeta(x=x, shape1 = alpha + K, shape2= beta + n - K)
MAP=x[which(posterior_prob==max(posterior_prob))]

plot(x,posterior_prob,type="l",main="Posterior Distribution")

a <- rbeta(n=1e6, shape1=alpha + K, shape2=beta + n - K)
hpd <- HPDinterval(as.mcmc(a), prob=0.95)
hpd
abline(v=hpd, lty=2)

print(paste("MAP=",MAP))
quantile(a)

#B
#10 samples
library(coda)
alpha <- 0.5
beta <- 0.5
n <- 10
K <- 2
x <- seq(0,1,0.01)

prior <- dbeta(x=x, shape1= alpha, shape2=beta)
par(mfrow=c(1,2))
plot(prior,main="Prior distribution",type="l")
likelihood <- dbinom(x=K, size=n, prob=x)
posterior_prob <- dbeta(x=x, shape1 = alpha + K, shape2= beta + n - K)
MAP=x[which(posterior_prob==max(posterior_prob))]
print(paste("MAP=",MAP))

plot(x,posterior_prob,type="l",main="Posterior Distribution")

a <- rbeta(n=1e6, shape1=alpha + K, shape2=beta + n - K)
hpd <- HPDinterval(as.mcmc(a), prob=0.95)
hpd
abline(v=hpd, lty=2)

quantile(a)

#100 samples
library(coda)
alpha <- 0.5
beta <- 0.5
n <- 100
K <- 20
x <- seq(0,1,0.01)

prior <- dbeta(x=x, shape1= alpha, shape2=beta)
par(mfrow=c(1,2))
plot(prior,main="Prior distribution",type="l")
likelihood <- dbinom(x=K, size=n, prob=x)
posterior_prob <- dbeta(x=x, shape1 = alpha + K, shape2= beta + n - K)
MAP=x[which(posterior_prob==max(posterior_prob))]


plot(x,posterior_prob,type="l",main="Posterior Distribution")

a <- rbeta(n=1e6, shape1=alpha + K, shape2=beta + n - K)
hpd <- HPDinterval(as.mcmc(a), prob=0.95)
hpd
abline(v=hpd, lty=2)

print(paste("MAP=",MAP))
quantile(a)

#changing from a uniform prior to a informative prior affected the distribution of the posterior. This
#resulted in a the quantiles becomes larger, indicating that the the distribution has stretched. Prior
#matters most for the sample size of 10 as the effect on the distribution will be greater. 

#C
library(coda)
alpha <- 0.5
beta <- 0.5
n <- 10
K <- 2
posterior_prob1 <- pbeta(0.5, shape1= alpha + K , shape2 = beta + n - K)
posterior_prob2 = 1 - posterior_prob1
ratio = posterior_prob1 / posterior_prob2
print(paste("BF=",ratio))



