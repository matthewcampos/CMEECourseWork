rm(list=ls())
d <- read.table("SparrowSize.txt", header=TRUE)
d1 <- subset(d, d$Tarsus!= "NA")
seTarsus <- sqrt(var(d1$Tarsus)/length(d1$Tarsus))
d12001 <- subset(d1, d1$Year==2001)
d12001
seTarsus2001 <- sqrt(var(d12001$Tarsus)/length(d12001$Tarsus))
seTarsus2001

#Exercises
#Tarsus SE
seTarsus_complete <- sqrt(var(d1$Tarsus)/length(d1$Tarsus)) #0.02096211
length(d1$Tarsus) #1685
CI <- (1.96*seTarsus_complete) #+-0.04108573
#Mass SE
d2 <- subset(d, d$Mass!="NA")
seMass_complete <- sqrt(var(d2$Mass)/length(d2$Mass)) #0.0513209
length(d2$Mass) #1704
CI2 <- 1.96*seMass_complete #+-0.100589
#Wing SE
d3 <- subset(d, d$Wing!="NA")
seWing_complete <- sqrt(var(d3$Wing)/length(d3$Wing)) #0.0586808
length(d3$Wing) #1695
CI3 <- 1.96*seWing_complete #+-0.1150144
#Bill SE
d4 <- subset(d, d$Bill!="NA")
seBill_complete <- sqrt(var(d4$Bill)/length(d4$Bill)) #0.01771217
length(d4$Bill) #1140
CI4 <- 1.96*seBill_complete #+-0.03471586
