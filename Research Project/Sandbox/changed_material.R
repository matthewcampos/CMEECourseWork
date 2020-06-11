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