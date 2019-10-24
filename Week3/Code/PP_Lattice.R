MyDF <- read.csv("../Data/EcolArchives-E089-51-D1.csv")
require(lattice)
require(plyr)

pdf("../Results/Pred_Lattice.pdf") #saves graph
Predator_mass_graph <- hist(log(MyDF$Predator.mass), main= "Histogram of Predator Mass", xlab="Predator Mass (g)", 
                            ylab="Count", col= "light blue", border = "black")
dev.off()

pdf("../Results/Prey_Lattice.pdf")
Prey_mass_graph <- hist(log(MyDF$Prey.mass), main= "Histogram of Prey Mass", xlab="Prey Mass (g)", 
                            ylab="Count", col= "red", border = "black")
dev.off()

pdf("../Results/SizeRatio_Lattice.pdf")
Size_ratio_graph <- hist(log((MyDF$Predator.mass/MyDF$Prey.mass)), main= "Histogram of Predator/Prey Size Ratio", xlab="Prey Mass (g)", 
                        ylab="Count", col= "white", border = "black")
dev.off()

dim(MyDF)
length(MyDF$Predator.mass)
colnames((MyDF))

#Calculating log of predator mass
predator_mass <- log(MyDF$Predator.mass)

#Calculating log of prey mass
prey_mass <- log(MyDF$Prey.mass)

#Calculating log of size ratio
size_ratio <- log((MyDF$Predator.mass)/(MyDF$Prey.mass))

#Dataframe to calculate median and mean
## ddply subsets the data based on the feeding interaction and summarises it 
calculation <- ddply(MyDF, .(Type.of.feeding.interaction), summarise,
                     mean.predator.mass = mean(predator_mass),
                     median.predator.mass = median(predator_mass),
                     mean.prey.mass = mean(prey_mass),
                     median.prey.mass = median(prey_mass),
                     mean.size.ratio = mean(size_ratio),
                     median.size.ratio = median(size_ratio))

#writes output to csv
write.csv(calculation, "../Results/PP_Results.csv")







