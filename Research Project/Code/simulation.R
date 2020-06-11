##Migration Simulation

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

source('functions.R')

#PACKAGES
library(deSolve) #for integral of ODE
library(zeallot) #for assigning variables

#ARGUMENTS
size <- 5 #population size
locus <- 12 #allele sites
generations <- 10

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
migrant_pop

#FOR CALCULATION OF Y VALUES
#randomly generate x_j values starting at 0
x_j <- data.frame(matrix(0, ncol = locus/2, nrow = size, byrow=TRUE))
colnames(x_j) <- c(11,21,31,12,22,32)
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
  #set 24 starting values
  for (j in 1:24){
    assign(variables[j],df_pop[,j])
  }
  #time to reach equilibrium values
  time <- seq(0, 200, by = 0.1)
  #initial state 
  X11 <- runif(size,0,1) #generate starting values where X11+X12 is y1 and so on...
  X12 <- runif(size,0,1)
  X21 <- runif(size,0,1)
  X22 <- runif(size,0,1)
  X31 <- runif(size,0,1)
  X32 <- runif(size,0,1)
  state <- array(c(X11,X12,X21,X22,X31,X32),dim = c(size,6))
  colnames(state) <- c('X11','X12','X21','X22','X31','X32')
  #Parameters
  parameters <-c(alpha11,alpha12,alpha21,alpha22,alpha31,alpha32,
                 gamma11,gamma12,gamma21,gamma22,gamma31,gamma32,
                 theta11,theta12,theta21,theta22,theta31,theta32,
                 P11,P12,P21,P22,P31,P32)
  #out order is- individual 1_11,2_11,...,n_11,1_12,2_12,..,n_12,1_21,...
  out <- ode(y=state,times=time,parms=parameters,func = y_model)
  #extract equilibrium values

  #PRODUCE NEXT GENERATION
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
  new_gen <- mutation(new_gen,0.05)
  print(new_gen)
  init_pop_array <- new_gen
  print(paste("Generation",i))
}

test <- init_pop_array[1:2,,]
test
rownames(test) <- c() #remove rownames
colnames(test) <- c() #remove colnames

time <- seq(0, 200, by = 0.1)
num_individuals <- 2
X11 <- runif(num_individuals,0,1) #generate starting values where X11+X12 is y1 and so on...
X12 <- runif(num_individuals,0,1)
X21 <- runif(num_individuals,0,1)
X22 <- runif(num_individuals,0,1)
X31 <- runif(num_individuals,0,1)
X32 <- runif(num_individuals,0,1)
state <- array(c(X11,X12,X21,X22,X31,X32),dim = c(num_individuals,6))
colnames(state) <- c('X11','X12','X21','X22','X31','X32')
#set starting values
alpha11=test[,1,1]
alpha12=test[,1,2]
alpha21=test[,5,1]
alpha22=test[,5,2]
alpha31=test[,9,1]
alpha32=test[,9,2]
gamma11=test[,2,1]
gamma12=test[,2,2]
gamma21=test[,6,1]
gamma22=test[,6,2]
gamma31=test[,10,1]
gamma32=test[,10,2]
theta11=test[,3,1]
theta12=test[,3,2]
theta21=test[,7,1]
theta22=test[,7,2]
theta31=test[,11,1]
theta32=test[,11,2]
P11=test[,4,1]
P12=test[,4,2]
P21=test[,8,1]
P22=test[,8,2]
P31=test[,12,1]
P32=test[,12,2]
parameters <-c(alpha11=test[,1,1],alpha12=test[,1,2],alpha21=test[,5,1],alpha22=test[,5,2],
               alpha31=test[,9,1],alpha32=test[,9,2],
               gamma11=test[,2,1],gamma12=test[,2,2],gamma21=test[,6,1],gamma22=test[,6,2],
               gamma31=test[,10,1],gamma32=test[,10,2],
               theta11=test[,3,1],theta12=test[,3,2],theta21=test[,7,1],theta22=test[,7,2],
               theta31=test[,11,1],theta32=test[,11,2],
               P11=test[,4,1],P12=test[,4,2],P21=test[,8,1],P22=test[,8,2],P31=test[,12,1],P32=test[,12,2])
out <- ode(y=state,times=time,parms=parameters,func = y_model)
#out order is- individual 1_11,2_11,...,n_11,1_12,2_12,..,n_12,1_21,...
tail(out)
#need last output once equilibrium reached








