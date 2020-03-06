##Additional investigation of temperature and medium

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

library(BisRNA)
library(stringr)
library(gridExtra)

rda_list <- list.files(path = "../Data/RDA",full.names = TRUE) #load RDA files

#Test for association between fit and medium
growth_rate_df <- data.frame(NA, ncol=4, nrow=length(rda_list))
for (len in 1:length(rda_list)){
  load(rda_list[len])
  growth_rate_df[len,1] <- ID[1]
  growth_rate_df[len,2] <- ID[2]
  growth_rate_df[len,3] <- as.numeric((ID[3]))
  growth_rate_df[len,4] <- as.numeric(r_value(data)[1])
}
colnames(growth_rate_df) <- c("Species", "Medium", "Temperature","Growth rate")

#remove NA
growth_rate_df <- na.omit(growth_rate_df) #remove NA

species_vect <- c() #to save each unique species 
medium_vect <- c() #each unique medium
cor_vect <- c() #each correlation value of temp and r for a unique species and medium 
p_value <- c() #p value of the correlation

#T0 for species
species_unique <- unique(growth_rate_df$Species) #vector of each the different species
index <- c()
pseudomonas_index <- str_detect(species_unique,regex("Pseudomonas sp.", ignore_case=TRUE)) #regex to find the species with conceptual temperature from paper
index <- c(which(pseudomonas_index==TRUE),index)
mesophilic_detect <- str_detect(species_unique,regex("mesophilic", ignore_case=TRUE))
index <- c(which(mesophilic_detect==TRUE),index) 
coli_detect <- str_detect(species_unique,"Escherichia coli")
index <- c(which(coli_detect==TRUE),index)
species_unique <- species_unique[index] #only get these species
rearrange <- order(species_unique) #make it alphabetic descending
species_unique <-species_unique[rearrange]
T0_index <- c(-0.25,1.85,-7.15,-8.05) #conceptual temperatures of the species
species_T0 <- data.frame(species_unique,T0_index,stringsAsFactors = FALSE) #combine species with conceptual temperatures
colnames(species_T0) <- c("Species", "Conceptual Temperature")

#export species_T0 as pdf
pdf("../Results/Species_ConceptualTemp.pdf",height = 3, width = 4)
grid.table(species_T0)
dev.off()

#temperature vs. growth rate plot for species
pdf(paste("../Results/Growth_vs_temp.pdf")) #open pdf
for (i in 1:length(species_unique)){
  growth_species_subset <- subset(growth_rate_df,growth_rate_df$Species == species_unique[i]) #filter each loop first by species
  medium_unique <- unique(growth_species_subset$Medium) #find the different mediums of the species
  for (j in 1:length(medium_unique)){
    growth_subset <- subset(growth_species_subset, growth_species_subset$Medium == medium_unique[j]) #produce a filtered table of species and specific medium
    reorder <- order(growth_subset$Temperature) #output is the index of increasing order of temperature
    growth_subset <- growth_subset[reorder,] #rearrange based on the index output
    species_vect <- c(growth_subset[1,1], species_vect)
    medium_vect <- c(growth_subset[1,2], medium_vect)
    growth_subset <- growth_subset[ ,c(-1,-2)] #remove unwanted column of species and medium
    growth_subset[,2] <- (growth_subset[,2])^0.5 
    lm_analysis <- lm(growth_subset$`Growth rate`~growth_subset$Temperature)
    analysis_summary <- summary(lm_analysis)
    x_intercept <- (0-analysis_summary$coefficients[1,1])/analysis_summary$coefficients[2,1]
    cor_vect <- c(analysis_summary$r.squared^0.5, cor_vect) #correlation is the square root
    p_value <- c((analysis_summary$coefficients[2,4]), p_value)
    plot(growth_subset$Temperature,growth_subset$`Growth rate`,main = paste(species_unique[i],"in",medium_unique[j]),xlab = "Temperature", ylab = "Max Growth Rates",pch=19) 
    x_axis <- seq(min(growth_subset$Temperature)-10,max(growth_subset$Temperature)+10)
    t <- x_axis - species_T0[i,2]
    y <- c()
    y <- c(analysis_summary$coefficients[2,1]*(t),y)
    lines(t,y,col="blue")
    abline(lm_analysis,col="red")
    legend("top", legend = paste("r=",analysis_summary$r.squared^0.5),box.lty = 0, cex = 0.6)
    legend("topleft", legend = paste("x intercept=",x_intercept),box.lty = 0, cex = 0.6)
    legend("bottomright", legend=c("Linear Regression", "Conceptual Temperature"),col=c("red", "blue"), lty=1:1, cex=0.6)  
  }
}
dev.off()

#fishers method for p-values
fisher.method(p_value)

#convert vectors to dataframe
cor_df <- data.frame(species_vect,medium_vect,cor_vect,p_value)
colnames(cor_df) <- c("Species", "Medium", "Correlation Coefficient", "Linear Regression Statistic")

#save correlation df as pdf
pdf("../Results/Temperature_Growthrate.pdf",height = 5, width = 8.5)
grid.table(cor_df)
dev.off()

#Example E.coli
pdf(paste("../Results/Ecoli_r_temp.pdf")) #open pdf
growth_species_subset <- subset(growth_rate_df,growth_rate_df$Species == species_unique[2]) #filter each loop first by species
medium_unique <- unique(growth_species_subset$Medium) #find the different mediums of the species
growth_subset <- subset(growth_species_subset, growth_species_subset$Medium == medium_unique[2]) #produce a filtered table of species and specific medium
reorder <- order(growth_subset$Temperature) #output is the index of increasing order of temperature
growth_subset <- growth_subset[reorder,] #rearrange based on the index output
species_vect <- c(growth_subset[1,1], species_vect)
medium_vect <- c(growth_subset[1,2], medium_vect)
growth_subset <- growth_subset[ ,c(-1,-2)] #remove unwanted column of species and medium
growth_subset[,2] <- (growth_subset[,2])^0.5 
lm_analysis <- lm(growth_subset$`Growth rate`~growth_subset$Temperature)
analysis_summary <- summary(lm_analysis)
x_intercept <- (0-analysis_summary$coefficients[1,1])/analysis_summary$coefficients[2,1]
cor_vect <- c(analysis_summary$r.squared^0.5, cor_vect) #correlation is the square root
p_value <- c((analysis_summary$coefficients[2,4]), p_value)
plot(growth_subset$Temperature,growth_subset$`Growth rate`,main = paste(species_unique[2],"in",medium_unique[2]),xlab = "Temperature", ylab = "Max Growth Rates",pch=19) 
x_axis <- seq(min(growth_subset$Temperature)-10,max(growth_subset$Temperature)+10)
t <- x_axis - species_T0[i,2]
y <- c()
y <- c(analysis_summary$coefficients[2,1]*(t),y)
lines(t,y,col="blue")
abline(lm_analysis,col="red")
legend("top", legend = paste("r=",analysis_summary$r.squared^0.5),box.lty = 0, cex = 0.6)
legend("topleft", legend = paste("x intercept=",x_intercept),box.lty = 0, cex = 0.6)
legend("bottomright", legend=c("Linear Regression", "Conceptual Temperature"),col=c("red", "blue"), lty=1:1, cex=0.6)  
dev.off()

