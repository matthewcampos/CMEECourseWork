#previous functions
fitness <- function(pop_array){
  #replaces the nucleotides with fitness values 
  row_pop_array <- dim(init_pop_array)[1] #saves number of row
  col_pop_array <- dim(init_pop_array)[2] #saves number of columns
  h_pop_array <- dim(init_pop_array)[3] #saves height
  fit_array <- init_pop_array
  fit_array[fit_array=="A"] <- fitness_vector[1] #replaces as.character
  fit_array[fit_array=="T"] <- fitness_vector[2]
  fit_array[fit_array=="C"] <- fitness_vector[3]
  fit_array[fit_array=="G"] <- fitness_vector[4]
  fit_array <- mapply(fit_array, FUN = as.numeric) #convert character to numeric
  fit_array <- array(data = fit_array, dim = c(row_pop_array,col_pop_array,h_pop_array))
  return(fit_array)
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

mutation <- function(new_gen,pr){
  probability <- runif(1,0,1)
  if (probability <= pr){
    individual <- sample(c(1:dim(new_gen)[1]),size = )
    site <- sample(c(1:dim(new_gen)[2]),size = 1)
    height <- sample(c(1:dim(new_gen)[3]),size = 1)
    new_gen[individual,site,height] <- new_gen[individual,site,height] + runif(1,-1,1)
  }
  return(new_gen)
}

mutation <- function(new_gen,pr){
  probability <- runif(1,0,1)
  if (probability <= pr){
    allele_sites <- floor(runif(1,1,dim(new_gen)[1]*dim(new_gen)[2]*dim(new_gen)[3])) #randomly choose alleles from whole matrix
    new_gen[sample(c(1:dim(new_gen)[1]*dim(new_gen)[2]*dim(new_gen)[3]),size = allele_sites),,] <- runif(1,0,1) 
  }
  return(new_gen)
}

test <- new_gen[1:5,,]
test[,1:12,] <- c(1,3,5,7)
test <- t(test)

y <- matrix(c(2,6,8), ncol = 6, nrow = 5, byrow=TRUE)
theta <- as.data.frame(test[,c(3,7,11),])
P <- as.data.frame(test[,c(4,8,12),])



R_j_value <- R_j(y,theta,P) 

X_j <- 0.5
alpha <- test[,c(1,5,9),]
gamma <- test[,c(2,6,10),]


d_X_value <- d_X_j(alpha,R_j_value,gamma,X_j) 

X_j <- X_j + d_X_value 

#theta values
theta <- as.data.frame(init_pop_array[,c(3,7,11),])
colnames(theta) <- c(11,21,31,12,22,32)
#P values
P <- as.data.frame(init_pop_array[,c(4,8,12),])
colnames(P) <- c(11,21,31,12,22,32)
#alpha values
alpha <- as.data.frame(init_pop_array[,c(1,5,9),])
colnames(alpha) <- c(11,21,31,12,22,32)
#gamma values
gamma <- as.data.frame(init_pop_array[,c(2,6,10),])
colnames(gamma) <- c(11,21,31,12,22,32)
#calculate R_ij values
R_11_value <- negative_R_j(y_j$`2`,theta$`21`,P$`21`)
R_12_value <- negative_R_j(y_j$`2`,theta$`22`,P$`22`)
R_21_value <- positive_R_j(y_j$`1`,theta$`11`,P$`11`)
R_22_value <- positive_R_j(y_j$`1`,theta$`12`,P$`12`)
R_31_value <- positive_R_j(y_j$`1`,theta$`31`,P$`31`)
R_32_value <- positive_R_j(y_j$`1`,theta$`32`,P$`32`)
#calculate d_x
d_x11_value <- d_x_j(alpha$`11`,gamma$`11`,x_j$`11`,R_11_value)
#calculate new x_j values
x_j <- x_j + d_x_value

#random generate y values 
y_j[,c(1,4)] <- x_j$`11` + x_j$`12` #y1- 1 value for each individual
y_j[,c(2,5)] <- x_j$`21` + x_j$`22` #y2
y_j[,c(3,6)] <- x_j$`31` + x_j$`32` #y3 update during development, do this until equilibrium reached before duing evolutionary simulation
#think of plants- grow before pollinating (minitime step (development) within generational timestep)
#add negative autoregulatory feedback- follow paper
print(y_j)

d_x_j <- function(alpha, gamma, x_j, R_j_value){
  #calcualte d_X values (change in x value)
  d_X_value <- (alpha * R_j_value) - (gamma * x_j)
  rownames(d_X_value) <- c() #remove row names
  colnames(d_X_value) <- c() #remove col names
  return(d_X_value)
}

y_model <- function (time, y, parms) {
  with(as.list(c(y, parms)),{
    R_11_value <- negative_R_j(X21,X22,theta21,P21)
    R_12_value <- negative_R_j(X21,X22,theta22,P22)
    R_21_value <- positive_R_j(X11,X12,theta11,P11)
    R_22_value <- positive_R_j(X11,X12,theta12,P12)
    R_31_value <- positive_R_j(X11,X12,theta31,P31)
    R_32_value <- positive_R_j(X11,X12,theta32,P32)
    dX11 <- (alpha11 * R_11_value) - (gamma11 * X11)
    dX12 <- (alpha12 * R_12_value) - (gamma12 * X12)
    dX21 <- (alpha21 * R_21_value) - (gamma21 * X21)
    dX22 <- (alpha22 * R_22_value) - (gamma22 * X22)
    dX31 <- (alpha31 * R_31_value) - (gamma31 * X31)
    dX32 <- (alpha32 * R_32_value) - (gamma32 * X32)
    list(c(dX11,dX12,dX21,dX22,dX31,dX32))
  })
}

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


test <- init_pop_array[1:2,,]
test

time <- seq(0, 200, by = 0.1)
num_individuals <- 2
X11 <- 0.1#runif(num_individuals,0,1) #generate starting values where X11+X12 is y1 and so on...
X12 <- 0.1#runif(num_individuals,0,1)
X21 <- 0.1#runif(num_individuals,0,1)
X22 <- 0.1#runif(num_individuals,0,1)
X31 <- 0.1#runif(num_individuals,0,1)
X32 <- 0.1#runif(num_individuals,0,1)
state <- array(c(X11,X12,X21,X22,X31,X32),dim = c(num_individuals,6))
colnames(state) <- c('X11','X12','X21','X22','X31','X32')
#set starting values
parameters = c(
  alpha11=test[1,1] ,
  alpha12=test[1,2] ,
  alpha21=test[5,1] ,
  alpha22=test[5,2] ,
  alpha31=test[9,1] ,
  alpha32=test[9,2] ,
  gamma11=test[2,1],
  gamma12=test[2,2],
  gamma21=test[6,1],
  gamma22=test[6,2],
  gamma31=test[10,1],
  gamma32=test[10,2],
  theta11=test[3,1],
  theta12=test[3,2],
  theta21=test[7,1],
  theta22=test[7,2],
  theta31=test[11,1],
  theta32=test[11,2],
  P11=test[4,1],
  P12=test[4,2],
  P21=test[8,1],
  P22=test[8,2],
  P31=test[12,1],
  P32=test[12,2]
)
parameters <-c(alpha11=test[,1,1],alpha12=test[,1,2],alpha21=test[,5,1],alpha22=test[,5,2],
               alpha31=test[,9,1],alpha32=test[,9,2],
               gamma11=test[,2,1],gamma12=test[,2,2],gamma21=test[,6,1],gamma22=test[,6,2],
               gamma31=test[,10,1],gamma32=test[,10,2],
               theta11=test[,3,1],theta12=test[,3,2],theta21=test[,7,1],theta22=test[,7,2],
               theta31=test[,11,1],theta32=test[,11,2],
               P11=test[,4,1],P12=test[,4,2],P21=test[,8,1],P22=test[,8,2],P31=test[,12,1],P32=test[,12,2])
out <- ode(y=state,times=time,parms=parameters,func = y_model)
#out order is- individual 1_11,2_11,...,n_11,1_12,2_12,..,n_12,1_21,...
head(out)
tail(out)
#need last output once equilibrium reached
#alpha*R can be smaller causing it to go negative
#time is affecting output
e <- equil_values(y=state,parms=parameters)

test
#using equilibrium solutions
alpha <- as.data.frame(test[,c(1,5,9),c(1,2)])
colnames(alpha) <- c(11,21,31,12,22,32)
gamma <- as.data.frame(test[,c(2,6,10),c(1,2)])
colnames(gamma) <- c(11,21,31,12,22,32)
theta <- as.data.frame(test[,c(3,7,11),c(1,2)])
colnames(theta) <- c(11,21,31,12,22,32)
P <- as.data.frame(test[,c(4,8,12),c(1,2)])
colnames(P) <- c(11,21,31,12,22,32)

mu <- mu_values(alpha,gamma)
y_1 <- (mu$`11` * negative_R_j(X21,X22,theta$`21`,P$`21`)) + (mu$`12` * negative_R_j(X21,X22,theta$`22`,P$`22`))
y_2 <- (mu$`21` * positive_R_j(X11,X12,theta$`11`,P$`11`)) + (mu$`22` * positive_R_j(X11,X12,theta$`12`,P$`12`))
y_3 <- (mu$`31` * positive_R_j(X11,X12,theta$`31`,P$`31`)) + (mu$`32` * positive_R_j(X11,X12,theta$`32`,P$`32`))

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

names <- rep(c('alpha','gamma','theta','P'),times=locus/2)
number <- rep(c(11,12,21,22,31,32),each=locus/3)
variables <- paste0(names,number)
migrant_variables <- paste0(names,number)

#set the 24 starting values
#for (m in 1:24){
#  assign(variables[m],df_pop[,m]) #init_pop
#  assign(migrant_variables[m],df_migrant_pop[,m]) #migrant_pop
#}

#track dominance over time- omholt paper- dominance function
#variance of parameter values
#each individual has 4 different locus (alpha,gamma,theta,P) and two sets (diploid)- look at values at each locus
#if ex. alpha11 and alpha12 have different (0.28 and 0.50), thats hetero and use y3 value
#produce y3 value for 0.28 and 0.28, and 0.50 and 0.50, and get the mean of those 
#dominance value is y3 values produced (hetero-mean(homo)/abs(hetero-mean(homo)))

dominance <- function(d_array){
  for (i in 1:dim(init_pop_array)[1]){
    #set the homozygotes
    homozygous_1 <- init_pop_array[i,,]
    homozygous_2 <- init_pop_array[i,,]
    for (j in 1:dim(homozygous_1)[2]){
      if (homozygous_1[j,1]!=homozygous_1[j,2]){ #heterozygous if not equal
        hetero_y_3 <- y_3[i] #y_value of heterozygote
        #re-distribute values
        homozygous_1[j,2] <- homozygous_1[j,1] 
        homozygous_2[j,1] <- homozygous_2[j,2]
        #collect values
        homo_1_alpha <- as.data.frame(homozygous_1[c(1,5,9),])
        homo_1_gamma <- as.data.frame(homozygous_1[c(2,6,10),])
        homo_1_theta <- as.data.frame(homozygous_1[c(3,7,11),])
        homo_1_P <- as.data.frame(homozygous_1[c(4,8,12),])
        homo_2_alpha <- as.data.frame(homozygous_2[c(1,5,9),])
        homo_2_gamma <- as.data.frame(homozygous_2[c(2,6,10),])
        homo_2_theta <- as.data.frame(homozygous_2[c(3,7,11),])
        homo_2_P <- as.data.frame(homozygous_2[c(4,8,12),])
        #mu values
        homo_1_mu <- mu_values(homo_1_alpha,homo_1_gamma)
        homo_2_mu <- mu_values(homo_2_alpha,homo_2_gamma)
        #y_values
        #y_3 only affected by locus in position 9 and beyond
        #homo_1_y_1 <- (homo_1_mu$V1[1] * negative_R_j(y_j$`2`[i],homo_1_theta$V1[2],homo_1_P$V1[2])) + (homo_1_mu$V2[1] * negative_R_j(y_j$`2`[i],homo_1_theta$V2[2],homo_1_P$V2[2]))
        #homo_1_y_2 <- (homo_1_mu$V1[2] * negative_R_j(y_j$`1`[i],homo_1_theta$V1[1],homo_1_P$V1[1])) + (homo_1_mu$V2[2] * negative_R_j(y_j$`1`[i],homo_1_theta$V2[1],homo_1_P$V2[1]))
        homo_1_y_3 <- (homo_1_mu$V1[3] * negative_R_j(y_j$`1`[i],homo_1_theta$V1[3],homo_1_P$V1[3])) + (homo_1_mu$V2[3] * negative_R_j(y_j$`1`[i],homo_1_theta$V2[3],homo_1_P$V2[3]))
        #homo_2_y_1 <- (homo_2_mu$V1[1] * negative_R_j(y_j$`2`[i],homo_2_theta$V1[2],homo_2_P$V1[2])) + (homo_2_mu$V2[1] * negative_R_j(y_j$`2`[i],homo_2_theta$V2[2],homo_2_P$V2[2]))
        #homo_2_y_2 <- (homo_2_mu$V1[2] * negative_R_j(y_j$`1`[i],homo_2_theta$V1[1],homo_2_P$V1[1])) + (homo_2_mu$V2[2] * negative_R_j(y_j$`1`[i],homo_2_theta$V2[1],homo_2_P$V2[1]))
        homo_2_y_3 <- (homo_2_mu$V1[3] * negative_R_j(y_j$`1`[i],homo_2_theta$V1[3],homo_2_P$V1[3])) + (homo_2_mu$V2[3] * negative_R_j(y_j$`1`[i],homo_2_theta$V2[3],homo_2_P$V2[3]))
      }
      #calculate dominance
      d <- (hetero_y_3 - mean(homo_1_y_3,homo_2_y_3)) / abs(hetero_y_3 - mean(homo_1_y_3,homo_2_y_3))
      d_array[i,j] <- d
    }
  }
}

#null simulation- prob is equal for all individuals
#max fitness individual per generation and 100 SNP mutate it (mutation function) generate new trait value
#robustness to genetic variation and without migration
#evolution different if migration per generation (0.01) or every 10 generations (0.1)

#max fit individual mutation
in_mutation <- function(array){
  mutation_array <- array(sample(runif(locus*3,0,1),locus*2,replace = TRUE), dim=c(dim(array)[1],dim(array)[2]))
  indices <- which(mutation_array <= mutation_rate, arr.ind = TRUE) #find indices less than mutation rate
  if (dim(indices)[1] > 0){
    array[indices] <- pmax(0.1,rnorm(dim(indices)[1],mean=array[indices],sd=0.001)) #values to add or subtract by
  }
  return(array)
}

heterogenous.fit.list <- list()
for (k in 1:8){
  path <- (paste0(heterogenous[k],'/run_1/Fitness/'))
  for (j in 1:15){
    load(paste0(path,'Simulation',j,'.rda'))
    heterogenous.fit.list[[length(heterogenous.fit.list)+1]] <- fit
  }
  array <- do.call(cbind, heterogenous.fit.list)
  mean.array <- apply(array, MARGIN = 1,mean, na.rm = TRUE)
  pdf(paste0("../Results/Avg_Heterogenous_Plot_of_Simulations.pdf")) #open pdf
  plot(1:length(mean.array),as.numeric(mean.array),type = 'l',xlab = 'Generations', ylab = 'Average Fitness')
  mtext("Average Fitness Progress of Heterogenous Population",side = 3, line = -2, outer = TRUE)
  dev.off()
}
homogenous.fit.list <- list()
for (m in 1:8){
  path <- (paste0(homogenous[m],'/run_1/Fitness/'))
  for (n in 1:15){
    load(paste0(path,'Simulation',n,'.rda'))
    homogenous.fit.list[[length(homogenous.fit.list)+1]] <- fit
  }
  homogenous.array <- do.call(cbind, homogenous.fit.list)
  homogenous.mean.array <- apply(homogenous.array, MARGIN = 1,mean, na.rm = TRUE)
  pdf(paste0("../Results/Avg_Homogenous_Plot_of_Simulations.pdf")) #open pdf
  plot(1:length(homogenous.mean.array),as.numeric(homogenous.mean.array),type = 'l',xlab = 'Generations', ylab = 'Average Fitness')
  mtext("Average Fitness Progress of Homogenous Population",side = 3, line = -2, outer = TRUE)
  dev.off()
}

#boxplot to compare migration and w/o migration
wo.migration <- data.frame(result$migration.rate,result$migration.pattern,result$max.speed,result$avg_fit)
rm_migration <- which(wo.migration$result.migration.rate==0 & wo.migration$result.migration.pattern==0)
wo.migration <- wo.migration[rm_migration,] #remove migration
w.migration <- data.frame(result$migration.rate,result$migration.pattern,result$max.speed,result$avg_fit)
w.migration <- w.migration[-rm_migration,] #remove no migration
matrix.both <- as.data.frame(matrix(NA,nrow = 2640,ncol = 3)) #for box plot
matrix.both[1:240,1] <- "Without Migration"
matrix.both[241:2640,1] <- "With Migration"
matrix.both[1:240,2] <- as.numeric(wo.migration$result.max.speed)
matrix.both[241:2640,2] <- as.numeric(w.migration$result.max.speed)
matrix.both[1:240,3] <- as.numeric(wo.migration$result.avg_fit)
matrix.both[241:2640,3] <- as.numeric(w.migration$result.avg_fit)
matrix.both$V1 <- factor(matrix.both$V1, levels = c('Without Migration','With Migration'))
boxplot(matrix.both$V2~matrix.both$V1,data = matrix.both,xlab = 'Conditions',ylab = 'Speed to Average Fitness')




