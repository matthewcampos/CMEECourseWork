##script to plot regression of predator mass and prey mass, subsetting by predator.lifestage and Type.of.feeding.interaction.

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

rm(list=ls())
MyDF <- read.csv("../Data/EcolArchives-E089-51-D1.csv")
names(MyDF) #get header names

require(ggplot2)
require(plyr)

#find all those prey mass in mg and change it to g
indexes=c()
length(MyDF$Prey.mass.unit)
for (i in 1:34931){
  if (MyDF$Prey.mass.unit[i]=="mg"){
    indexes=c(indexes,i)
  }
}
print(indexes)
length(indexes)

length(MyDF$Prey.mass)
for (x in 1:34931){
  for (y in 1:203){
    if (x==indexes[y]){
      MyDF$Prey.mass[x]=MyDF$Prey.mass[x]/1000
      MyDF$Prey.mass.unit[y]= "g"
    }
  }
}

#generate plot with certain points and log data
plot <- 
qplot(Prey.mass, Predator.mass, data=MyDF, log="xy", colour= Predator.lifestage, shape=I(3), xlab= "Prey mass in grams", ylab= "Predator mass in grams") +
#produce different graphs based on feeding interaction
facet_grid(MyDF$Type.of.feeding.interaction) +
#add regression line
geom_smooth(method = "lm", fullrange=TRUE) + 
#edit theme and size
guides(colour = guide_legend(nrow = 1))+
theme_bw()+
theme(legend.position="bottom", 
      legend.title = element_text(face="bold", size=8),
      legend.text = element_text(size=6),
      strip.text.y = element_text(size=7)) +
coord_fixed(ratio=0.2) 

#save plot
pdf("../Results/PP_Regress.pdf")
print(plot)
dev.off()

#output and save to different file
Results <- ddply(
  MyDF, .(Type.of.feeding.interaction, Predator.lifestage), summarize,
  #Intercept of lm
  lm_intercept = summary(lm(MyDF$Predator.mass ~ MyDF$Prey.mass)) $coef[1,1],
  #Slope of lm
  lm_slope = summary(lm(MyDF$Predator.mass ~ MyDF$Prey.mass)) $coef[2,1],
  #r squared value
  lm_r_squared = summary(lm(MyDF$Predator.mass ~ MyDF$Prey.mass)) $r.squared,
  #F statistic
  F_statistic = summary(lm(MyDF$Predator.mass ~ MyDF$Prey.mass)) $fstatistic[1],
  #P value
  P_value = pf(summary(lm(MyDF$Predator.mass ~ MyDF$Prey.mass)) $fstatistic[1],
               summary(lm(MyDF$Predator.mass ~ MyDF$Prey.mass)) $fstatistic[2],
               summary(lm(MyDF$Predator.mass ~ MyDF$Prey.mass)) $fstatistic[3],
               lower.tail = F) 
  )
write.csv(Results, "../Results/PP_Regress.csv")



