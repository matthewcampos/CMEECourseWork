require(minpack.lm)
powMod <- function(x, a, b){
  return(a*x^b)
}

MyData <- read.csv("../Data/GenomeSize.csv")
head(MyData)

Data2Fit <- subset(MyData, Suborder=="Anisoptera")
Data2Fit <- Data2Fit[!is.na(Data2Fit$TotalLength),] #remove NA's

plot(Data2Fit$TotalLength, Data2Fit$BodyWeight)

library(ggplot2)
ggplot(Data2Fit, aes(x= TotalLength, y= BodyWeight)) +
geom_point(size=(3), color="red") + theme_bw() +
labs(y="Body mass (mg)", x="Wing Length (mm)")

PowFit <- nlsLM(BodyWeight ~ powMod(TotalLength, a, b), data = Data2Fit, start = list(a = .1, b = .1)) #have to manually enter values for a&b
summary(PowFit)
Lengths <- seq(min(Data2Fit$TotalLength),max(Data2Fit$TotalLength),len=200) #generates regular sequence
coef(PowFit)["a"] #retrieves coefficients
coef(PowFit)["b"]



Predic2PlotPow <- powMod(Lengths, coef(PowFit)["a"], coef(PowFit)["b"])
plot(Data2Fit$TotalLength, Data2Fit$BodyWeight) #text(0.2,30, labels="3.94e-06*x^2.59"))
lines(Lengths, Predic2PlotPow, col = 'blue', lwd = 2.5)
#line_equation <- function(){
  #eq <- substitute(italic(y) == a * italic(x)^b,
                   #list(a=coef(PowFit)["a"],
                        #b=coef(PowFit)["b"]))
  #as.character(as.expression(eq))
#}
confint(PowFit) #computes confidence intervals for one or more parameters in a fitted model

#Comparing models
QuaFit <- lm(BodyWeight ~ poly(TotalLength,2), data = Data2Fit) #degree of 2
Predic2PlotQua <- predict.lm(QuaFit, data.frame(TotalLength=Lengths))
plot(Data2Fit$TotalLength, Data2Fit$BodyWeight)
lines(Lengths, Predic2PlotPow, col = 'blue', lwd = 2.5)
lines(Lengths, Predic2PlotQua, col = 'red', lwd = 2.5)

RSS_Pow <- sum(residuals(PowFit)^2)  # Residual sum of squares
TSS_Pow <- sum((Data2Fit$BodyWeight - mean(Data2Fit$BodyWeight))^2)  # Total sum of squares
RSq_Pow <- 1 - (RSS_Pow/TSS_Pow)  # R-squared value

RSS_Qua <- sum(residuals(QuaFit)^2)  # Residual sum of squares
TSS_Qua <- sum((Data2Fit$BodyWeight - mean(Data2Fit$BodyWeight))^2)  # Total sum of squares
RSq_Qua <- 1 - (RSS_Qua/TSS_Qua)  # R-squared value

RSq_Pow 
RSq_Qua

n <- nrow(Data2Fit) #set sample size
pPow <- length(coef(PowFit)) # get number of parameters in power law model
pQua <- length(coef(QuaFit)) # get number of parameters in quadratic model

AIC_Pow <- n + 2 + n * log((2 * pi) / n) +  n * log(RSS_Pow) + 2 * pPow
AIC_Qua <- n + 2 + n * log((2 * pi) / n) + n * log(RSS_Qua) + 2 * pQua
AIC_Pow - AIC_Qua

AIC(PowFit) - AIC(QuaFit)









