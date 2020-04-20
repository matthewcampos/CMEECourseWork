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