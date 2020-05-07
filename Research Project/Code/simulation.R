##Migration Simulation

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'
rm(list=ls()) #clear workspace

#ARGUMENTS
size <- 100 #population size
locus <- 6 #allele sites
generations <- 3

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

#Mutation 
mutation <- function(new_gen,pr){
  probability <- runif(1,0,1)
  if (probability <= pr){
    allele_sites <- floor(runif(1,1,dim(new_gen)[1]*dim(new_gen)[2]*dim(new_gen)[3])) #randomly choose alleles from whole matrix
    new_gen[sample(c(1:dim(new_gen)[1]*dim(new_gen)[2]*dim(new_gen)[3]),size = allele_sites),,] <- runif(1,0,1) 
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

#SIMULATION
#initial population
init_pop_array <- population(size) 

#TEST randomly generate nucleotide sequences
for (i in 1:dim(init_pop_array)[1]){
  for (j in 1:dim(init_pop_array)[3]){
    random_sequence <- sample(runif(10,0,1),6,replace = TRUE)
    init_pop_array[i,,j] <- random_sequence
  }
}
init_pop_array

#Loop through many generations
for (i in 1:generations){
  #Determine Fitness
  fit_array <- fitness(init_pop_array)
  fit_array
  
  #matrix of parent combinations 
  parents <- matrix(NA,nrow = size*2, ncol = 2) #matrix of individual indexes
  parents[,1] <- as.matrix(sample(seq(1,size,1),size*2, replace = TRUE, prob = fit_array))
  parents[,2] <- as.matrix(sample(seq(1,size,1),size*2, replace = TRUE, prob = fit_array))
  
  #remove repeating parents in same row
  parents <- parents[!parents[,1]==parents[,2],]
  
  #randomly choose 100 of the combinations
  parents <- parents[sample(1:nrow(parents), size),]
  
  #Next generation population array
  new_gen <- population(size)
  
  #make individuals for next generation
  for (k in 1:size){
    #array of the two parents
    parent_1 <- init_pop_array[parents[k,1],,] #retrieves index from parents matrix
    parent_2 <- init_pop_array[parents[k,2],,]
    height_1 <- cbind(parent_1[,1],parent_2[,1])
    height_2 <- cbind(parent_1[,2],parent_2[,2])
    #assign nucleotides to individuals
    for (j in 1:dim(new_gen)[2]){
      if (height_1[j,1] == height_1[j,2] | height_2[j,1] == height_2[j,2]){
        new_gen[k,j,1] <- height_1[j]
        new_gen[k,j,2] <- height_2[j]
      }else if (height_1[j,1] != height_1[j,2] | height_2[j,2] != height_2[j,2]){
        new_gen[k,j,1] <- sample(height_1[j,],size = 1)
        new_gen[k,j,2] <- sample(height_2[j,],size = 1)
      }
    }
    #Recombination
    new_gen[k,,] <- recombination(new_gen[k,,])
  }
  #Mutation
  new_gen <- mutation(new_gen,0.2)
  print(new_gen)
  init_pop_array <- new_gen
  print(paste("Generation",i))

}





