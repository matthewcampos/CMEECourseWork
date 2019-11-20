rm(list=ls())
d <- read.table("SparrowSize.txt", header=TRUE)
boxplot(d$Mass~d$Sex.1, col=c("red","blue"), ylab="Body Mass (g)")
t.test <- t.test(d$Mass~d$Sex.1)
t.test
d1 <- as.data.frame(head(d,50))
t.test2 <- t.test(d1$Mass~d1$Sex)
t.test2

d2 <- subset(d,d$Year==2001)
mean_wing <- mean(d$Wing, na.rm=TRUE)

t.test3 <- t.test(d2$Wing, mu= mean_wing, na.rm=TRUE)

t.test4 <- t.test(d2$Wing~d2$Sex.1)

t.test5 <- t.test(d$Tarsus~d$Sex.1)

require(pwr)
sd <- sqrt(var(d$Wing, na.rm=TRUE))
pwr.t.test(d=5/sd,power=0.8,sig.level = 0.05,type = "one.sample",alternative = "two.sided")
