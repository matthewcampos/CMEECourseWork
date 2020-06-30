##Migration Simulation

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

source('functions.R')

#PACKAGES
library(deSolve) #for integral of ODE
library(zeallot) #for assigning variables

#keep population size constant

#ARGUMENTS
size <- 100 #population size
locus <- 12 #allele sites
generations <- 500
#mu_bar #mean for gaussian fitness distribution
sigma <- 3 #sd for gaussian fitness distribution
mutation_rate <- 0.01 #determine if locus undergoes mutation
mu_bar <- 50 #desired fitness value

#List for fitness
w_bar_list <- data.frame(matrix(NA,nrow = generations,ncol = 1))
colnames(w_bar_list) <- 'average fitness'

#SIMULATION
#initial population
init_pop_array <- population(size,locus) #main population 
migrant_pop <- population(size,locus)

#TEST randomly generate nucleotide sequences
for (i in 1:dim(init_pop_array)[1]){
  for (j in 1:dim(init_pop_array)[3]){
    init_pop_array[i,,j] <- sample(runif(locus+5,0,1),locus,replace = TRUE)
    migrant_pop[i,,j] <- sample(runif(locus+5,0,1),locus,replace = TRUE)
  }
}
init_pop_array

#FOR CALCULATION OF Y VALUES
#randomly generate y_j values starting with 3 values
y_j <- data.frame(matrix(runif(3,0,1), ncol = locus/4, nrow = size, byrow=TRUE))
colnames(y_j) <- c(1,2,3)

names <- rep(c('alpha','gamma','theta','P'),times=locus/2)
number <- rep(c(11,12,21,22,31,32),each=locus/3)
variables <- paste0(names,number)

#Loop through many generations
for (i in 1:generations){
  #CALCULATE Y FOR CURRENT GENERATION
  #collect values
  df_pop <- as.data.frame(init_pop_array)
  colnames(df_pop) <- variables
  #set the 24 starting values
  for (m in 1:24){
    assign(variables[m],df_pop[,m])
  }
  #collect values
  alpha <- as.data.frame(init_pop_array[,c(1,5,9),c(1,2)])
  colnames(alpha) <- c(11,21,31,12,22,32)
  gamma <- as.data.frame(init_pop_array[,c(2,6,10),c(1,2)])
  colnames(gamma) <- c(11,21,31,12,22,32)
  theta <- as.data.frame(init_pop_array[,c(3,7,11),c(1,2)])
  colnames(theta) <- c(11,21,31,12,22,32)
  P <- as.data.frame(init_pop_array[,c(4,8,12),c(1,2)])
  colnames(P) <- c(11,21,31,12,22,32)
  
  #mu values- ratio of alpha and gamma
  mu <- mu_values(alpha,gamma)
  
  #y values representing trait values to determine fitness probability
  y_1 <- (mu$`11` * negative_R_j(y_j$`2`,theta$`21`,P$`21`)) + (mu$`12` * negative_R_j(y_j$`2`,theta$`22`,P$`22`))
  y_2 <- (mu$`21` * positive_R_j(y_j$`1`,theta$`11`,P$`11`)) + (mu$`22` * positive_R_j(y_j$`1`,theta$`12`,P$`12`))
  y_3 <- (mu$`31` * positive_R_j(y_j$`1`,theta$`31`,P$`31`)) + (mu$`32` * positive_R_j(y_j$`1`,theta$`32`,P$`32`))
  
  #average fitness- use y_3 value 
  if (i == 1){
    start_value <- mean(y_3)
  }
  w_bar_list[i,] <- mean(y_3)
  w_bar_list
  
  #PRODUCE NEXT GENERATION 
  fit_array <- fitness(y_3, mu_bar, sigma)
  
  #matrix of parent combinations 
  parents <- matrix(NA,nrow = size*2, ncol = 2) #matrix of individual indexes
  parents[,1] <- as.matrix(sample(seq(1,size,1),size*2, replace = TRUE, prob = fit_array))
  parents[,2] <- as.matrix(sample(seq(1,size,1),size*2, replace = TRUE, prob = fit_array))
  
  #remove repeating parents in same row
  parents <- parents[!parents[,1]==parents[,2],]
  
  #randomly choose set from the combinations
  parents <- parents[sample(1:nrow(parents), size=size,replace = TRUE),]
  
  #Next generation population array
  new_gen <- population(size,locus)
  
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
  #new_gen <- mutation(new_gen,mutation_rate)
  init_pop_array <- new_gen
  print(paste("Generation",i))
}









