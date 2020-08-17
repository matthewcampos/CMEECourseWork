##Evolution Functions 132.239.70.109

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

#Determine fitness function-begin with directional then switch into stabilising using Cauchy Distribution
#Cauchy distribution is a good indicator of how sensitive the tests are to heavy-tail departures from normality
fitness <- function(y_3,mu_bar,sigma){
  probabilities <- dcauchy(y_3, location = mu_bar, scale = sigma)
  return(probabilities)
}

#Mutation using array to determine which locus undergo mutation
mutation <- function(array,mutation_rate){
  mutation_array <- population(size,locus) #create mutation array 
  #generate runif mutation probabilities for each locus
  for (i in 1:dim(mutation_array)[1]){
    for (j in 1:dim(mutation_array)[3]){
      mutation_array[i,,j] <- sample(runif(locus,0,1),locus,replace = TRUE)
    }
  }
  indices <- which(mutation_array <= mutation_rate, arr.ind = TRUE) #find indices less than mutation rate
  #record old value
  allele_matrix <<- matrix(NA,nrow = dim(indices)[1],ncol = 2)
  if (dim(indices)[1] > 0){
    allele_matrix[,1] <<- array[indices]
    new_values <- pmax(0.1,rnorm(dim(indices)[1],mean=array[indices],sd=0.05))
    array[indices] <- new_values #new value centered around old value
    allele_matrix[,2] <<- new_values
  }else{
    allele_matrix <<- 0
  }
  return(array)
}

#Recombination- https://www.blackwellpublishing.com/ridley/a-z/Recombination.asp
recombination <- function(individual){
  copy <- individual
  site <- ceiling(runif(1,1.1,locus-0.1)) #determines which locus where it occurs
  individual[site:locus,1] <- copy[site:locus,2] #switches alleles
  individual[site:locus,2] <- copy[site:locus,1] 
  return(individual)
}

#MIGRATION 
migration <- function(array,migrant_array,migration_rate){
  #matrix to runif values for migrant population- whicever less than or equal to migration rate migrates
  #pop size of original array kept constant
  migrant_prob <- matrix(runif(size,0,1),nrow = size, ncol = 1)
  migrant_individuals <- which(migrant_prob <= migration_rate)
  init_individuals <- sample(size, length(migrant_individuals)) #choose individual to replace by migrants
  column <<- cbind(init_individuals,migrant_individuals)
  array[init_individuals,,] <- migrant_array[migrant_individuals,,]
  return(array)
}

#CALCULATE TRAIT VALUES 
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










