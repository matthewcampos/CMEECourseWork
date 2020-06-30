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

#Determine fitness function- normal distribution setting mean and s.d i.e. want trait value to center around 100
#begin with directional then switch into stabilising i.e. while y_3 > 4 sd from mean fitness = y_3 then once within use dnorm
#dcauchy
fitness <- function(y_3,mu_bar,sigma){
  probabilities <- dcauchy(y_3, location = mu_bar, scale = sigma)
  return(probabilities)
}

#Mutation- normal distribution and random determine to add or subtract around current value
#runif of 2400 (dim of array) and whichever values less than mutation rate undergo mutation
mutation <- function(new_gen,mutation_rate){
  mutation_array <- population(size,locus) #create mutation array 
  #generate runif mutation probabilities for each locus
  for (i in 1:dim(mutation_array)[1]){
    for (j in 1:dim(mutation_array)[3]){
      mutation_array[i,,j] <- sample(runif(locus,0,1),locus,replace = TRUE)
    }
  }
  indices <- which(mutation_array <= mutation_rate, arr.ind = TRUE) #find indices less than mutation rate
  if (dim(indices)[1] > 0){
    probability <- runif(dim(indices)[1],0,1) #determine whether to add or subtract
    subtraction <- which(probability <= 0.5) #indexes (individuals) to subtract 
    addition <- which(probability > 0.5) 
    values <- runif(dim(indices)[1],0,init_pop_array[indices]) #values to add or subtract by
    print(values)
    if (length(subtraction) > 0){
      init_pop_array[indices[subtraction]] <- init_pop_array[indices[subtraction]] - values[subtraction]
    }
    if (length(addition) > 0){
      init_pop_array[indices[addition]] <- init_pop_array[indices[addition]] + values[addition]
   }
   }
  return(init_pop_array)
}

#Recombination- https://www.blackwellpublishing.com/ridley/a-z/Recombination.asp
recombination <- function(individual){
  copy <- individual
  site <- ceiling(runif(1,1.1,locus-0.1)) #determines which locus where it occurs
  individual[site:locus,1] <- copy[site:locus,2] #switches alleles
  individual[site:locus,2] <- copy[site:locus,1] 
  return(individual)
}

#FORMULAS TO CALCULATE TRAIT VALUES 
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







