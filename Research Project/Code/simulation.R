##Migration Simulation

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'
rm(list=ls()) #clear workspace

#ARGUMENTS
allele_vector <- c(0.8,0.45,0.23,0.05)
size <- 4 #population size
locus <- 6 #allele sites

#FUNCTIONS
#Create array of individuals and their genotypes
population <- function(size, locus=6){
  #initialise starting population array containing genotypes where size is the number of individuals 
  #and locus are the allele sites
  pop <- array(NA, dim = c(size,locus,2))
  return(pop)
}

#Determine fitness function
fitness <- function(pop_array){
  fit_sum <- apply(pop_array,1,sum) #sums across the rows 
  return(fit_sum)
}

#Assign probabilities based on fitness
probabilities <- function(fit_sum){
  #fitness as a fraction of total representing probabilities
  total <- sum(fit_sum) 
  values <- c()
  for (i in fit_sum){
    values <- c(values,(i/total))
  }
  return(values)
}

#Mutation 
mutation <- function(new_gen,pr){
  probability <- runif(1,0,1)
  if (probability <= pr){
    individual <- sample(c(1:dim(new_gen)[1]),size = 1)
    site <- sample(c(1:dim(new_gen)[2]),size = 1)
    height <- sample(c(1:dim(new_gen)[3]),size = 1)
    new_gen[individual,site,height] <- new_gen[individual,site,height] + runif(1,-1,1)
  }
  return(new_gen)
}

#SIMULATION
#initial population
init_pop_array <- population(size) 

#TEST randomly generate nucleotide sequences
for (i in 1:dim(init_pop_array)[1]){
  for (j in 1:dim(init_pop_array)[3]){
    seq <- sample(allele_vector,6,replace = TRUE)
    init_pop_array[i,,j] <- seq
  }
}
init_pop_array

#Loop through many generations
for (i in 1:3){
  #Determine Fitness
  fit_array <- fitness(init_pop_array)
  fit_array
  
  #Assign probabilities based on fitness
  probs <- probabilities(fit_array)
  probs
  
  #Next generation Simulation
  new_gen <- population(size)
  #choose two individuals to use their genotype for next generation
  for (i in 1:size){
    #choose two parents for an individual in next gen
    adult_row <- sample(nrow(init_pop_array),size = 2, replace = FALSE, prob = probs) #choose two parents using row number
    #array of the two parents
    parent_1 <- init_pop_array[adult_row[1],,]
    parent_2 <- init_pop_array[adult_row[2],,]
    height_1 <- cbind(parent_1[,1],parent_2[,1])
    height_2 <- cbind(parent_1[,2],parent_2[,2])
    #assign nucleotides to individuals
    for (j in 1:dim(new_gen)[2]){
      if (height_1[j,1] == height_1[j,2] | height_2[j,1] == height_2[j,2]){
        new_gen[i,j,1] <- height_1[j]
        new_gen[i,j,2] <- height_2[j]
      }else if (height_1[j,1] != height_1[j,2] | height_2[j,2] != height_2[j,2]){
        new_gen[i,j,1] <- sample(height_1[j,],size = 1)
        new_gen[i,j,2] <- sample(height_2[j,],size = 1)
      }
    }
    new_gen <- mutation(new_gen,0.2)
  }
  print(new_gen)
  init_pop_array <- new_gen
}





