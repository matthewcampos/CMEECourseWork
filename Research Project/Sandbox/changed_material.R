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