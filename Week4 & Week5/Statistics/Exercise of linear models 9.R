x <- c(1,2,3,4,8)
y <- c(4,3,5,7,9)
model1 <- (lm(y~x))
summary(model1) #t test and p value of slope showing the effect of x on y and, t test and p value of intercept showing if significantly different from 0
anova(model1) #slope is for an increase in 1 x, we can y increase in y
resid(model1)
cov(x,y)
var(x)
plot(y~x)
abline(model1)
