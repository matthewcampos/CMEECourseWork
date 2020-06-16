##Migration Simulation 132.239.70.109

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'
rm(list=ls()) #clear workspace

#FUNCTIONS
#Create array of individuals and their genotypes
population <- function(size, locus){
  #initialise starting population array containing genotypes where size is the number of individuals 
  #and locus are the allele sites
  pop <- array(NA, dim = c(size,locus,2))
  #alpha is protein production rate
  #gamma is decay rate
  words <- rep(c("alpha","gamma","theta","P"),times = locus/4)
  colnames(pop) <- words
  return(pop)
}

#Determine fitness function
fitness <- function(y_1,y_2,y_3){
  fit <- cbind(y_1,y_2,y_3)
  fit_sum <- apply(fit,1,sum) #sums across the rows 
  return(fit_sum)
}

#Mutation 
mutation <- function(new_gen,pr){
  probability <- runif(1,0,1)
  if (probability <= pr){
    individuals <- sample(c(1:dim(new_gen)[1]),size = sample(dim(new_gen)[1]),replace = TRUE)
    site <- sample(c(1:dim(new_gen)[2]),replace = TRUE)
    height <- sample(c(1:dim(new_gen)[3]),size = length(individuals),replace = TRUE)
    new_gen[individuals,site,height] <- runif(1,0,1) 
  }
  return(new_gen)
}

#Recombination- https://www.blackwellpublishing.com/ridley/a-z/Recombination.asp
recombination <- function(individual){
  copy <- individual
  site <- ceiling(runif(1,1.1,locus-0.1)) #determines which locus where it occurs
  individual[site:locus,1] <- copy[site:locus,2] #switches alleles
  individual[site:locus,2] <- copy[site:locus,1] 
  return(individual)
}

#FORMULAS 
positive_R_j <- function(y,theta,P){
  #calculate R value
  R_j_value <- (y^P)  / ((y^P) + (theta^P))
  rownames(R_j_value) <- c()  #remove row names
  colnames(R_j_value) <- c() #remove col names
  return(R_j_value)
}

negative_R_j <- function(y,theta,P){
  #calculate R value
  R_j_value <- 1- ((y^P)  / ((y^P) + (theta^P)))
  rownames(R_j_value) <- c()  #remove row names
  colnames(R_j_value) <- c() #remove col names
  return(R_j_value)
}

mu_values <- function(alpha, gamma){
  #ratio of alpha and gamma
  mu <- alpha/gamma
  return(mu)
}







