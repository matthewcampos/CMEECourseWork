##Simulation

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

#each run has 100 simulations and folder naming order is:
#(hetero/homo)main population, (hetero/homo)migrant population, Migration rate, Every 10 or not

#Environmental distance- keep main pop constant but migrant 35,65,80, Genetic network- same regulations or switched, Migration rate- 0%,1%-5% intervals of 0.5, Migration pattern- random or every x gen

source('functions.R')

#PACKAGES
library(zeallot) #for assigning variables
#set.seed(789)
args = commandArgs(trailingOnly=TRUE)
#creates directory folder
subfolder_name <- paste0("run_",c(1:1))
mainDir <- '../SAMEGN_50-65'
folder_name <- paste0(args[4],"-",args[5],"_Migration_",args[2],"-",args[3])
path <- file.path(mainDir,folder_name,subfolder_name)
for (run in 1:1){
  print(paste0('run',run))
  #Create directories
  fitness_folder <- dir.create(paste0(path[run],'/','Fitness'), recursive = TRUE) #create fitness folder within run folder
  trait_folder <- dir.create(paste0(path[run],'/','Traits'), recursive = TRUE) #create traits folder within run folder
  pop_folder <- dir.create(paste0(path[run],'/','Population'), recursive = TRUE)
  parent_folder <- dir.create(paste0(path[run],'/','Parents'), recursive = TRUE)
  allele_folder <- dir.create(paste0(path[run],'/','Alleles'), recursive = TRUE)
  migrant_folder <-dir.create(paste0(path[run],'/','Migrants'), recursive = TRUE)
  #pdf(paste0(path[run],'/',"Plot_of_Simulations.pdf")) #open pdf
  count <- 0 #check that simulation is working 
  for (loop in 1:30){
    #keep population size constant
    count <- count + 1
    print(count)
    #ARGUMENTS
    #commnd line inputs
    size <- as.numeric(500)#as.numeric(args[1]) #population size 
    migration_rate <- as.numeric(0.05) #as.numeric(args[2]) #0 is none except for every == 5
    every <- as.numeric(0)#as.numeric(args[3]) #migration 0== every gen, 1== every 10 gen 2== every 5, 3== random
    hetero_homo <- 'heterozygous'#args[4] #heterogenous or homogenous population
    mig_hetero_homo <- 'homozygous'  #args[5] #heterogenous or homogenous migrant population
    chance.record <- c() #vector to save random migration times
    #set variables 
    locus <- 12 #allele sites per strand 
    generations <- 1200 
    mutation_rate <- 1.1e-3 #determine if locus undergoes mutation
    mu_bar <- 50 #desired fitness value
    sigma <- 8 #3 sd for gaussian fitness distribution
    migrant_mu_bar <- 65 #100
    migrant_sigma <- 10 #5
    #traits array to track trait value per generation
    w_bar_list <- data.frame(matrix(NA,nrow = generations,ncol = 1))
    colnames(w_bar_list) <- 'average trait'
    migrant_w_bar_list <- data.frame(matrix(NA,nrow = generations,ncol = 1))
    colnames(migrant_w_bar_list) <- 'average trait'
    #fitness array to track fitness per generation
    fit <- data.frame(matrix(NA,nrow = generations,ncol = 1))
    colnames(fit) <- 'average fitness'
    #MAIN SIMULATION
    #initial populations
    init_pop_array <- population(size,locus) #main population 
    migrant_pop <- population(size,locus) #migrant
    #Main population
    if (hetero_homo == 'homozygous'){
      for (p in 1:dim(init_pop_array)[1]){
        for (q in 1:dim(init_pop_array)[3]){
          init_pop_array[p,,q] <- sample(runif(1,0,0.3),locus,replace = TRUE)
        }
      }
    }else if (hetero_homo == 'heterozygous'){
      for (p in 1:dim(init_pop_array)[1]){
        for (q in 1:dim(init_pop_array)[3]){
          init_pop_array[p,,q] <- sample(runif(locus+5,0,0.3),locus,replace = TRUE)
        }
      }
    }
    #migrant population
    if (mig_hetero_homo == 'homozygous'){
      for (t in 1:dim(init_pop_array)[1]){
        for (y in 1:dim(init_pop_array)[3]){
          migrant_pop[t,,y] <- sample(runif(1,0,0.3),locus,replace = TRUE)
        }
      }
    }else if (mig_hetero_homo == 'heterozygous'){
      for (t in 1:dim(init_pop_array)[1]){
        for (y in 1:dim(init_pop_array)[3]){
          migrant_pop[t,,y] <- sample(runif(locus+5,0,0.3),locus,replace = TRUE)
        }
      }
    }
    #FOR CALCULATION OF Y VALUES
    #randomly generate y_j values starting with 3 values
    if (hetero_homo == 'heterozygous'){
      y_j <- data.frame(matrix(runif(size*3,0,1), ncol = locus/4, nrow = size, byrow=TRUE))
      colnames(y_j) <- c(1,2,3)
    }else{
      y_j <- data.frame(matrix(runif(3,0,1), ncol = locus/4, nrow = size, byrow=TRUE))
      colnames(y_j) <- c(1,2,3)
    }
    if (mig_hetero_homo == 'heterozygous'){
      migrant_y_j <- data.frame(matrix(runif(size*3,0,1), ncol = locus/4, nrow = size, byrow=TRUE))
      colnames(migrant_y_j) <- c(1,2,3)
    }else{
      migrant_y_j <- data.frame(matrix(runif(3,0,1), ncol = locus/4, nrow = size, byrow=TRUE))
      colnames(migrant_y_j) <- c(1,2,3)
    }
    #labels for locus
    names <- rep(c('alpha','gamma','theta','P'),times=locus/2)
    number <- rep(c(11,12,21,22,31,32),each=locus/3)
    variables <- paste0(names,number)
    #List to track Migrants
    migrant.list <- list()
    #List for allele mutations
    allele.mutation.list <- list()
    #List to save populations
    pop.list <- list()
    migrantpop.list <- list()
    heading <- paste0("Generation",0)
    #add starting population
    pop.list[[heading]] <- init_pop_array
    migrantpop.list[[heading]] <- migrant_pop
    #List for parents
    parents.list <- list()
    #List to save y_j values
    #add starting y_values
    y1_values <- list(y_j$X1)
    y2_values <- list(y_j$X2)
    y3_values <- list(y_j$X3)
    mig_y1_values <- list(migrant_y_j$`1`)
    mig_y2_values <- list(migrant_y_j$`2`)
    mig_y3_values <- list(migrant_y_j$`3`)
    #Loop through many generations
    for (i in 1:generations){
      #replace heading for pop list
      heading <- paste0("Generation",i)
      #CALCULATE Y FOR CURRENT GENERATION
      #collect values
      alpha <- as.data.frame(init_pop_array[,c(1,5,9),c(1,2)])
      colnames(alpha) <- c(11,21,31,12,22,32)
      gamma <- as.data.frame(init_pop_array[,c(2,6,10),c(1,2)])
      colnames(gamma) <- c(11,21,31,12,22,32)
      theta <- as.data.frame(init_pop_array[,c(3,7,11),c(1,2)])
      colnames(theta) <- c(11,21,31,12,22,32)
      P <- as.data.frame(init_pop_array[,c(4,8,12),c(1,2)])
      colnames(P) <- c(11,21,31,12,22,32)
      migrant_alpha <- as.data.frame(migrant_pop[,c(1,5,9),c(1,2)])
      colnames(migrant_alpha) <- c(11,21,31,12,22,32)
      migrant_gamma <- as.data.frame(migrant_pop[,c(2,6,10),c(1,2)])
      colnames(migrant_gamma) <- c(11,21,31,12,22,32)
      migrant_theta <- as.data.frame(migrant_pop[,c(3,7,11),c(1,2)])
      colnames(migrant_theta) <- c(11,21,31,12,22,32)
      migrant_P <- as.data.frame(migrant_pop[,c(4,8,12),c(1,2)])
      colnames(migrant_P) <- c(11,21,31,12,22,32)
      #mu values- ratio of alpha and gamma
      mu <- mu_values(alpha,gamma)
      migrant_mu <- mu_values(migrant_alpha,migrant_gamma)
      #y values representing trait values to determine fitness probability
      #y_3 positively regulated by y_1 which is negatively regulated by y_2
      y_1 <- (mu$`11` * negative_R_j(y_j$`2`,theta$`21`,P$`21`)) + (mu$`12` * negative_R_j(y_j$`2`,theta$`22`,P$`22`))
      y_2 <- (mu$`21` * positive_R_j(y_j$`1`,theta$`11`,P$`11`)) + (mu$`22` * positive_R_j(y_j$`1`,theta$`12`,P$`12`))
      y_3 <- (mu$`31` * positive_R_j(y_j$`1`,theta$`31`,P$`31`)) + (mu$`32` * positive_R_j(y_j$`1`,theta$`32`,P$`32`))
      #y_3 positively regulated by y_2 which is negatively regulated by y_1
      migrant_y_1 <- (migrant_mu$`11` * negative_R_j(migrant_y_j$`2`,migrant_theta$`21`,migrant_P$`21`)) + (migrant_mu$`12` * negative_R_j(migrant_y_j$`2`,migrant_theta$`22`,migrant_P$`22`))
      migrant_y_2 <- (migrant_mu$`21` * positive_R_j(migrant_y_j$`1`,migrant_theta$`11`,migrant_P$`11`)) + (migrant_mu$`22` * positive_R_j(migrant_y_j$`1`,migrant_theta$`12`,migrant_P$`12`))
      migrant_y_3 <- (migrant_mu$`31` * positive_R_j(migrant_y_j$`1`,migrant_theta$`31`,migrant_P$`31`)) + (migrant_mu$`32` * positive_R_j(migrant_y_j$`1`,migrant_theta$`32`,migrant_P$`32`))
      #replace y values
      y_j$`1` <- y_1
      y_j$`2` <- y_2
      y_j$`3` <- y_3
      migrant_y_j$`1` <- migrant_y_1
      migrant_y_j$`2` <- migrant_y_2
      migrant_y_j$`3` <- migrant_y_3
      w_bar_list[i,] <- mean(y_3)
      migrant_w_bar_list[i,] <- mean(migrant_y_3)
      #save y_values to list
      y1_values[[length(y1_values)+1]] <- y_1
      y2_values[[length(y2_values)+1]] <- y_2
      y3_values[[length(y3_values)+1]] <- y_3
      mig_y1_values[[length(mig_y1_values)+1]] <- migrant_y_1
      mig_y2_values[[length(mig_y2_values)+1]] <- migrant_y_2
      mig_y3_values[[length(mig_y3_values)+1]] <- migrant_y_3
      #PRODUCE NEXT GENERATION 
      fit_array <- fitness(y_3, mu_bar, sigma)
      migrant_fit_array <- fitness(migrant_y_3, migrant_mu_bar, migrant_sigma)
      #save average fitness for main populatoin- use y_3 value 
      fit[i,] <- mean(fit_array)
      #matrix of parent combinations for both populations
      #null simulation without probability
      #probability represents selection
      parents <- array(NA, dim = c(size*2,2,2)) #array of individual indexes
      parents[,1,1] <- as.matrix(sample(seq(1,size,1),size*2, replace = TRUE,prob = fit_array))
      parents[,2,1] <- as.matrix(sample(seq(1,size,1),size*2, replace = TRUE,prob = fit_array))
      parents[,1,2] <- as.matrix(sample(seq(1,size,1),size*2, replace = TRUE,prob = migrant_fit_array))
      parents[,2,2] <- as.matrix(sample(seq(1,size,1),size*2, replace = TRUE,prob = migrant_fit_array))
      #remove repeating parents in same row
      init_parents <- parents[!parents[,1,1]==parents[,2,1],,1] #first array
      migrant_parents <- parents[!parents[,1,2]==parents[,2,2],,2] #migrant pop
      #randomly choose set from the combinations
      init_parents <- init_parents[sample(1:nrow(init_parents), size=size,replace = TRUE),]
      migrant_parents <- migrant_parents[sample(1:nrow(migrant_parents), size=size,replace = TRUE),]
      #List of parents
      parents.list[[length(parents.list)+1]] <- list(init_parents)
      #Next generation population array
      new_gen <- population(size,locus)
      migrant_new_gen <- population(size,locus)
      #make individuals for next generation
      for (k in 1:size){
        #array of the two parents
        init_parent_1 <- init_pop_array[init_parents[k,1],,] #retrieves index from parents matrix
        init_parent_2 <- init_pop_array[init_parents[k,2],,]
        height_1 <- cbind(init_parent_1[,1],init_parent_2[,1])
        height_2 <- cbind(init_parent_1[,2],init_parent_2[,2])
        migrant_parent_1 <- migrant_pop[migrant_parents[k,1],,]
        migrant_parent_2 <- migrant_pop[migrant_parents[k,2],,]
        m_height_1 <- cbind(migrant_parent_1[,1],migrant_parent_2[,1])
        m_height_2 <- cbind(migrant_parent_1[,2],migrant_parent_2[,2])
        #assign nucleotides to individuals
        for (j in 1:dim(new_gen)[2]){
          if (height_1[j,1] == height_1[j,2]){#if values same
            new_gen[k,j,1] <- height_1[j]
          }else if (height_1[j,1] != height_1[j,2]){ #if values differ 
            new_gen[k,j,1] <- sample(height_1[j,],size = 1) #sample to decide which value offspring adopts 
          }
        }
        for (g in 1:dim(new_gen)[2]){
          if (height_2[g,1] == height_2[g,2]){
            new_gen[k,g,2] <- height_2[g]
          }else if (height_2[g,1] != height_2[g,2]){
            new_gen[k,g,2] <- sample(height_2[g,],size = 1)
          }
        }
        #migrant population
        for (m in 1:dim(migrant_new_gen)[2]){
          if (m_height_1[m,1] == m_height_1[m,2]){
            migrant_new_gen[k,m,1] <- m_height_1[m]
          }else if (m_height_1[m,1] != m_height_1[m,2]){
            migrant_new_gen[k,m,1] <- sample(m_height_1[m,],size = 1)
          }
        }
        for (n in 1:dim(migrant_new_gen)[2]){
          if (m_height_2[n,1] == m_height_2[n,2]){
            migrant_new_gen[k,n,2] <- m_height_2[n]
          }else if (m_height_2[n,1] != m_height_2[n,2]){
            migrant_new_gen[k,n,2] <- sample(m_height_2[n,],size = 1)
          }
        }
      }
      #Recombination
      new_gen[k,,] <- recombination(new_gen[k,,])
      migrant_new_gen[k,,] <- recombination(migrant_new_gen[k,,])
      #Mutation
      new_gen <- mutation(new_gen,mutation_rate)
      migrant_new_gen <- mutation(migrant_new_gen,mutation_rate)
      #List to record when theres a mutation- record old value
      allele.mutation.list[[length(allele.mutation.list)+1]] <- list(allele_matrix)
      #Migration after burn-in period
      #if every 10 generations
      if (i > 80 & i<=700){ #burn-in period
        if (migration_rate == 0 & every == 3){
          chance <- runif(1,0,1) #random
          if (chance >= 0.5){
            chance.record <- c(i,chance.record) #save vector
            local_rate <- runif(1,0.01,0.05) #between 1% and 5% migration rate
            new_gen <- migration(new_gen,migrant_new_gen,migration_rate=local_rate)
            migrant.list[[length(migrant.list)+1]] <- list(column)
          }
        }else if (migration_rate > 0){
          if (every == 0){
            new_gen <- migration(new_gen,migrant_new_gen,migration_rate) #each gen
            migrant.list[[length(migrant.list)+1]] <- column
          }else if (every == 1 & i %% 10 == 0){
            new_gen <- migration(new_gen,migrant_new_gen,migration_rate) #every 10
            migrant.list[[length(migrant.list)+1]] <- column
          }else if (every == 2 & i %% 5 == 0){
            new_gen <- migration(new_gen,migrant_new_gen,migration_rate) #every 5
            migrant.list[[length(migrant.list)+1]] <- column
          }else if (migration_rate == 0 & every == 0) {} #no migration
        }
      }
      #replace initial population each generation
      init_pop_array <- new_gen
      migrant_pop <- migrant_new_gen
      #add to pop.list
      if (i == 80 | i >= 700){
        pop.list[[length(pop.list)+1]] <- new_gen
        migrantpop.list[[length(migrantpop.list)+1]] <- migrant_new_gen
      }
    }
    #remove NA values and normalise
    fit <- na.omit(fit)
    fit <- fit/dcauchy(mu_bar,mu_bar,sigma) #normalise
    w_bar_list <- na.omit(w_bar_list)
    #plot
    #plot(c(1:dim(fit)[1]), fit[,1], type = 'l',xlab="Generations", ylab="Fitness")
    #list at end to save y_values as lists within list
    yvalues.list <- list(y1_values,y2_values,y3_values)
    mig_yvalues.list <- list(mig_y1_values,mig_y2_values,mig_y3_values)
    #save data- fitness and trait arrays, mean, max
    filename <- paste0(path[run],'/','Fitness/','Simulation',loop,".rda",sep="")
    save(fit,chance.record,file = filename) 
    file <- paste0(path[run],'/','Traits/','Simulation',loop,".rda",sep="")
    save(w_bar_list,yvalues.list,mig_yvalues.list,file = file)
    pop_file <- paste0(path[run],'/','Population/','Simulation',loop,".rda",sep="")
    save(pop.list,migrantpop.list,file=pop_file)
    parent_file <-paste0(path[run],'/','Parents/','Simulation',loop,".rda",sep="")
    save(parents.list,file=pop_file)
    allele_file <- paste0(path[run],'/','Alleles/','Simulation',loop,".rda",sep="")
    save(allele.mutation.list,file=pop_file)
    migrant_file <- paste0(path[run],'/','Migrants/','Simulation',loop,".rda",sep="")
    save(migrant.list,file=pop_file)
  }
  #dev.off() #close pdf file with all graphs
}
