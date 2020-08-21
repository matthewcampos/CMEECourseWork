x <- c(1:120)
val <- dcauchy(x,50,8)
val2 <- dcauchy(x,65,10)
val3 <- dcauchy(x,80,10)

plot(x,val,type = 'l',col='black',xlab='Trait Values',ylab='Fitness Probability',main='Cauchy Distribution of Environmental Distance',ylim=c(0,0.045))
lines(x,val2, type ='l',col='red')
lines(x,val3,type='l',col='blue')
legend(80, 0.0465, legend=c("Trait Value 50", "Trait Value 65",'Trait Value 80'),
       col=c("black", "blue",'red'), lty=1,cex=0.8)
