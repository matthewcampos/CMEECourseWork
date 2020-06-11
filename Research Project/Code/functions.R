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
fitness <- function(pop_array){
  fit_sum <- apply(pop_array,1,sum) #sums across the rows 
  return(fit_sum)
}

#Mutation 
mutation <- function(new_gen,pr){
  probability <- runif(1,0,1)
  if (probability <= pr){
    individuals <- sample(c(1:dim(new_gen)[1]),size = sample(dim(new_gen)[1]))
    site <- sample(c(1:dim(new_gen)[2]))
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
positive_R_j <- function(x_j1,x_j2,theta,P){
  y <- x_j1 + x_j2
  #calculate R value
  R_j_value <- (y^P)  / ((y^P) + (theta^P))
  rownames(R_j_value) <- c()  #remove row names
  colnames(R_j_value) <- c() #remove col names
  return(R_j_value)
}

negative_R_j <- function(x_j1,x_j2,theta,P){
  y <- x_j1 + x_j2
  #calculate R value
  R_j_value <- 1- ((y^P)  / ((y^P) + (theta^P)))
  rownames(R_j_value) <- c()  #remove row names
  colnames(R_j_value) <- c() #remove col names
  return(R_j_value)
}

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



