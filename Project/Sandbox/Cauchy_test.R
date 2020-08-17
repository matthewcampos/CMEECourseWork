x <- c(1:120)
val <- dcauchy(x,50,8)
val2 <- dcauchy(x,35,10)

plot(x,val,type = 'l',col='blue',ylim = c(0,0.1))
lines(x,val2, type ='l',col='red')
