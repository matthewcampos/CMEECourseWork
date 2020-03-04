# CMEE 2019 HPC excercises R code main 
name <- "Matthew Campos"
preferred_name <- "Matt"
email <- "matthew.campos19@imperial.ac.uk"
username <- "mlc19"
personal_speciation_rate <- 0.003874 # will be assigned to each person individually in class and should be between 0.002 and 0.007

# Question 1
species_richness <- function(community){
  richness <- length(unique(community)) #unique counts number of unique elements (species)
  return(richness)
}

# Question 2
init_community_max <- function(size){
  max <- seq(from = 1, to = size, by = 1) #prints vector starting from 1 to size increments of 1
  return(max)
}

# Question 3
init_community_min <- function(size){
  min <- seq(from = 1, to = 1, length.out = size) #prints out 1 size many times
  return(min)
}

# Question 4
choose_two <- function(max_value){
  x <- c(1:max_value)
  vect <- sample(x, 2, replace = FALSE) #extracts 2 values from the x vector without replacement so number not repeated
  return(vect)
}

# Question 5
neutral_step <- function(community){
  index <- choose_two(length(community)) #the values returned are indexes of the species in community
  community[index[1]] <- community[index[2]] #replaces the first index value with that in the second index value
  return(community)
}

# Question 6
neutral_generation <- function(community){
  x <- length(community)
  x <- x/2 #determines how many death, births for 1 generation
  val <- runif(1,0,1) #produces a random number between 0 and 1
  if (val >= 0.5){ #if 0.5 or greater than round up for the situation where x%%2 != 0
    x <- ceiling(x) 
  }
  else { #else round down
    x <- floor(x)
  }
  for (i in 1:x){
    community <- neutral_step(community) #replaces community each iteration after each death and birth
  }
  return(community)
}

# Question 7
neutral_time_series <- function(community,duration) {
  vec <- (species_richness(community)) #records initial species richness
  for (i in 1:duration){ #how many generations to run for
    community <- neutral_generation(community) #replaces community each generation
    vec <- c(vec,species_richness(community))
  }
  return(vec)
}

# Question 8
question_8 <- function() {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  y <- neutral_time_series(community = init_community_max(100),200)
  x <- c(1:201)
  plot(x, y, main="Time Series Graph of Neutral Model", ylab="Species Richness", xlab="Generations", type="l")
  legend(x = 150, y = 85, legend = c("Initialise Max"), lty=1,col = c("black"), cex=0.8)
  return("With a sufficient number of generations, the system will always converge to mono-dominance where all individuals are from one species")
}

# Question 9
neutral_step_speciation <- function(community,speciation_rate)  {
  index <- choose_two(length(community)) #the values returned are indexes of the species in community
  new_species <- max(community) + 1 #new species 
  determine <- runif(1,0,1) #number produced between 0-1 to decide if speciation occurs
  if (determine <= speciation_rate){ 
    community[index[1]] <- new_species #speciation occurs if determine is equal or less than speciation rate
  }
  else{
    community[index[1]] <- community[index[2]]
  }
  return(community)
}

# Question 10
neutral_generation_speciation <- function(community,speciation_rate)  {
  x <- length(community)
  x <- x/2 #determines how many death, births for 1 generation
  val <- runif(1,0,1) #produces a random number between 0 and 1
  if (val >= 0.5){ #if 0.5 or greater than round up
    x <- ceiling(x) 
  }
  else { #else round down
    x <- floor(x)
  }
  for (i in 1:x){
    community <- neutral_step_speciation(community,speciation_rate) #replaces community each iteration after each death and birth
  }
  return(community)
}

# Question 11
neutral_time_series_speciation <- function(community,speciation_rate,duration)  {
  vec <- (species_richness(community)) #records initial species richness
  for (i in 1:duration){ #how many generations to run for
    community <- neutral_generation_speciation(community,speciation_rate) #replaces community each generation
    vec <- c(vec,species_richness(community))
  }
  return(vec)
}

# Question 12
question_12 <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  y <- neutral_time_series_speciation(community = init_community_max(100),0.1,200)
  y_2 <- neutral_time_series_speciation(community = init_community_min(100),0.1,200)
  x <- c(1:201)
  plot(x, y, main="Time Series Graph of Neutral Model", ylab="Species Richness", xlab="Generations", type="l", col="red")
  lines(x, y_2, col="blue", type="l")
  legend(x = 120, y = 100, legend = c("Initialise Max", "Initialise Min"), col = c("red","blue"),lty=1:2, cex=0.8)
  return("With speciation, it prevents mono-dominance from persisting as new species can occur in both scenarios. Furthermore, the species richness is influenced by the speciation rate and number of individuals which is why they converge to a dynamic equilibrium, and have a similar pattern.")
}

# Question 13
species_abundance <- function(community)  {
  ordered <- sort(community) #saves community vector in increasing order
  a_count <- table(community) #counts the number of times each unique species occurs in a vector
  abundance_count <- as.numeric(sort(a_count,decreasing = TRUE)) #saves the counts
  return(abundance_count)
}

# Question 14
octaves <- function(abundance_vector) {
  result <- tabulate(floor(log2(abundance_vector))+1) #adding 1 ensures that 0's are read while log groups them
  return(result)
}

# Question 15
sum_vect <- function(x, y) {
  diff <- length(x) - length(y)
  if (diff > 0){
      y <- c(y, rep(0, diff))
  }
  if (diff < 0){
      x <- c(x, rep(0, abs(diff)))
  }
  res <- x + y
  return(res)
}

# Question 16 
question_16 <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  #setting initial parameters
  community <- init_community_max(100)
  community_2 <- init_community_min(100)
  speciation_rate <- 0.1
  for (i in 1:200){ #burn in period of 200 generations
    burn_in <- neutral_generation_speciation(community, speciation_rate) #returning the species at the end of each generation
    burn_in_2 <- neutral_generation_speciation(community_2, speciation_rate)
    community <- burn_in #replaces each generation so the next iteration is based of t-1 gen
    community_2 <- burn_in_2
  }
  count <- 0
  octave_vect <- octaves(species_abundance(community)) #set octaves of burn in generations
  octave_vect_2 <- octaves(species_abundance(community_2))
  for (j in 1:2000){ #run 2000 simulations after burn in
    count <- count + 1
    x <- neutral_generation_speciation(community, speciation_rate)
    y <- neutral_generation_speciation(community_2, speciation_rate)
    community <- x
    community_2 <- y
    if (count %% 20 == 0){
      octave_vect <- sum_vect(octave_vect,octaves(species_abundance(community))) #record every 20 generations
      octave_vect_2 <- sum_vect(octave_vect_2,octaves(species_abundance(community_2)))
    }
  }
  par(mfrow=c(1,2))
  barplot(octave_vect <- octave_vect/100, col="red", main="Species Abundance Octave Vector of Max", cex.main=0.8)
  barplot(octave_vect_2 <- octave_vect_2/100, col="blue", main="Species Abundance Octave Vector of Min", cex.main=0.8)
  return("Initial condition of the system does not affect end result because both have a poisson distribution shape with a cluster of species within a certain octave bin range that dominates. This is because species richness is affected by speciation rate and number of individuals. Both conditions have the same speciation rate so will follow the same trend.")
}

# Question 17
cluster_run <- function(speciation_rate, size, wall_time, interval_rich, interval_oct, length_burn_in_generations, output_file_name)  {
  community <- init_community_min(size) #set initial community
  #apply neutral generations with speciation for burn-in period
  time_series <- c() #species richness vector
  octave_list <- list() #empty list to append octaves
  final_community <- c() #vector for final community makeup
  ptm <- proc.time()[3] #start time
  count <- 0
  while (proc.time()[3] - ptm <= (wall_time * 60)){ #runs loop while run time is within wall_time minutes
    community <- neutral_generation_speciation(community,speciation_rate)
    count <- count + 1
    if (count %% interval_rich == 0 && count <= length_burn_in_generations) { #only records time series during burn in
      time_series <- c(species_richness(community),time_series)
    }
    if (count %% interval_oct == 0){ #records octaves throughout 
      o_vec <- octaves(species_abundance(community))
      octave_list <- append(octave_list, list(o_vec)) #adds to list
    }
  }
  final_community <- community
  parameters <- c(speciation_rate, size, wall_time, interval_rich, interval_oct, length_burn_in_generations, output_file_name)
  total_time <- proc.time()[3] - ptm
  save(time_series, octave_list, final_community, total_time, parameters,file = output_file_name)
}

# Questions 18 and 19 involve writing code elsewhere to run your simulations on the cluster

# Question 20 
process_cluster_results <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  gen_count <- list(0,0,0,0)
  names(gen_count) <- c(500,1000,2500,5000)
  aggregate_octave_lists <- list(list(), list(), list(), list()) #4 lists for the different sizes
  names(aggregate_octave_lists) <- c(500,1000,2500,5000)
  for (i in 1:100){
    load(paste0("RDA_results/Cluster_run_iteration_",i,"_.rda"))
    burn <- as.numeric(parameters[6])/as.numeric(parameters[5]) + 2 #how many times oct was recorded during burn in period
    oct_sum <- c(0,0)
    for (j in octave_list[burn:length(octave_list)]){ #removes burn in period
      oct_sum <- sum_vect(oct_sum,unlist(j)) #sum and makes into vector
    }
    aggregate_octave_lists[[toString(parameters[2])]] <- c(aggregate_octave_lists[[toString(parameters[2])]], list(oct_sum)) #saves sum octaves to a vector
    gen_count[[toString(parameters[2])]] <- gen_count[[toString(parameters[2])]] + (length(octave_list) - burn) #adds generation count to list
  }
  final_octaves_500 <- c(0,0) #makes it vector instead of int
  final_octaves_1000 <- c(0,0)
  final_octaves_2500 <- c(0,0)
  final_octaves_5000 <- c(0,0)
  for (k in 1:25){
    final_octaves_500 <- sum_vect(unlist(aggregate_octave_lists[["500"]][k]), final_octaves_500)
    final_octaves_1000 <- sum_vect(unlist(aggregate_octave_lists[["1000"]][k]), final_octaves_1000)
    final_octaves_2500 <- sum_vect(unlist(aggregate_octave_lists[["2500"]][k]), final_octaves_2500)
    final_octaves_5000 <- sum_vect(unlist(aggregate_octave_lists[["5000"]][k]), final_octaves_5000)
  }
  #Average
  final_octaves_500 <- final_octaves_500/gen_count[["500"]]
  final_octaves_1000 <- final_octaves_1000/gen_count[["1000"]]
  final_octaves_2500 <- final_octaves_2500/gen_count[["2500"]]
  final_octaves_5000 <- final_octaves_5000/gen_count[['5000']]
  #Plot
  par(mfrow=c(2,2))
  barplot(final_octaves_500, xlab="Octaves", cex.names=0.8, ylab="Average Species Abundance", main="Size 500", col = "grey")
  barplot(final_octaves_1000, xlab="Octaves", cex.names=0.8, ylab="Average Species Abundance", main="Size 1000", col = "blue")
  barplot(final_octaves_2500, xlab="Octaves", cex.names=0.8, ylab="Average Species Abundance", main="Size 2500", col = "red")
  barplot(final_octaves_5000, xlab="Octaves", cex.names=0.8, ylab="Average Species Abundance", main="Size 5000", col = "green")
  combined_results <- list(final_octaves_500,
                           final_octaves_1000,
                           final_octaves_2500,
                           final_octaves_5000) #create your list output here to return
  save(combined_results, file = "mlc19_cluster_results.rda")
  #return(combined_results)
}

# Question 21
question_21 <- function()  {
  x <- log(8)/log(3)
  print(x)
  return(c("The dimensions of this object is:",x,".Since we are calculating the dimensions of the black area, we can see that the pattern repeats three times in the width of the square. Hence, the width is 3. The size is the width raised to the power x, where x is the dimensions. Since, there are eight overall recurring patterns so the size is 8. Rearranging the formula, x equals the log of 8 divided by the log of 3."))
}

# Question 22
question_22 <- function()  {
  x <- log(20)/log(3)
  print(x)
  return("This is similar to the previous however we must think in a 3-D perspective. Rather than looking at each side, it is easier to consider the hypercube as consisting of smaller cubes with the same pattern. Thus, we split the hypercube into thirds vertically and see that there are three cubes per side, and 8 for the whole size. However, since the middle is hollow, we elimate 4 potential cubes, giving a total of 20 rather than 24. Same algebra function is performed.")
}

# Question 23
chaos_game <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  loc <- matrix(nrow = 4, ncol = 2, byrow = TRUE)
  row.names(loc) = c("A","B","C","X")
  colnames(loc)=c('x','y')
  loc[1,] <- c(0,0)
  loc[2,] <- c(3,4)
  loc[3,] <- c(4,1)
  loc[4,] <- c(0,0)
  plot(x=loc[,1],y=loc[,2])
  points.default(x=loc[4,1],y=loc[4,2],type='p',pch = 4,cex = 0.5)
  A <- c(0,0)
  B <- c(3,4)
  C <- c(4,1)
  X <- c(0,0)
  for (i in 1:50000){
    prob <- runif(1,min = 0,max = 1)
    vec <- c()
    if (prob <= 1/3){
      vec <- A
    }else if (prob >=2/3){
      vec <- B
    }else{
      vec <- C
    }
    X[1] <- (X[1] + vec[1])/2
    X[2] <- (X[2] + vec[2])/2
    points.default(x = X[1], y = X[2],type = 'p',pch = 4, cex = 0.08)
  }
  return("The fractal formed is Sierpinki's gasket with size of 3 and length of 2. The dimensions of this is log(3)/log(2). The reason for the hollow spaces is because there are only 3 points to which the point vector X can move to. For example, if point vector X is half-way between (0,0) and (4,1), it only has three possibilities of locations to move to. Either towards (0,0), (4,1) or (3,4). The empty spaces represent the area which it cannot move as there are no points for x to move to.")
}

# Question 24
plot(0:5,0:5,type='n')
turtle <- function(start_position, direction, length)  {
  end_position1 <- length * cos(direction)
  end_position2 <- length * sin(direction)
  segments(start_position[1],start_position[2], start_position[1]+end_position1,start_position[2]+end_position2)
  return(c(start_position[1]+end_position1,start_position[2]+end_position2)) # you should return your endpoint here.
}

# Question 25
plot(0:5,0:5,type='n')
elbow <- function(start_position, direction, length)  {
  new_start <- turtle(start_position,direction, length)
  new_direction <- direction - (pi/4)
  new_length <- length*0.95
  turtle(new_start,new_direction,new_length)
  return("elbow draws a second line using the end point of the turtle function as the new starting point. Printing it 45 degrees to the right of initial line drawn.")
}

# Question 26
spiral <- function(start_position, direction, length)  {
  new_start <- turtle(start_position,direction, length)
  new_direction <- direction - (pi/4)
  new_length <- length*0.95
  if (new_length<=0.005){
    return("By calling on spiral rather than turtle again, it is like going to top of the spiral function and running turtle repeatedly, without having to use a for loop. It produces a spiral using the elbow conditions, however because it gets so small the error is that r cannot draw the line anymore since it is tending towards size 0.")
  }
  return(spiral(new_start,new_direction,new_length))
}

# Question 27
draw_spiral <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  plot(0:5,0:5,type='n')
  spiral(c(0,3),pi/4,2)
}

# Question 28
tree <- function(start_position, direction, length)  {
  new_start <- turtle(start_position,direction, length)
  new_direction <- direction - (pi/4)
  new_length <- length * 0.65
  other_direction <- direction + (pi/4)
  if (new_length<=0.005){
    return()
  }
  return(c(tree(new_start,new_direction,new_length),(tree(new_start,other_direction,new_length))))
}
draw_tree <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  plot(0:6,0:6,type='n')
  tree(c(3,0),pi/2,2)
}

# Question 29
fern <- function(start_position, direction, length)  {
  new_start <- turtle(start_position,direction, length)
  new_direction <- direction + (pi/4)
  new_length <- length * 0.38
  other_length <- length * 0.87
  other_direction <- direction 
  if (new_length<=0.002){
    return()
  }
  return(c(fern(new_start,new_direction,new_length),(fern(new_start,other_direction,other_length))))
}
  
draw_fern <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  plot(0:5,0:5,type='n')
  fern(c(0,0),pi/4,0.7)
}

# Question 30
plot(0:5,0:5,type='n')
fern2 <- function(start_position, dir, direction, length)  {
  new_start <- turtle(start_position, direction, length)
  dir <- dir * -1
  new_direction <- direction + dir * (pi/4) #moves to the left 
  new_length <- length * 0.38
  other_length <- length * 0.87
  other_direction <- direction 
  if (new_length>=0.001){
    fern2(new_start,-dir,new_direction,new_length)
    fern2(new_start,dir,other_direction,other_length)
  }

}
draw_fern2 <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  plot(0:6,0:6,type='n')
  dir <- -1
  fern2(c(3,0),dir,pi/2,0.7) 
}

# Challenge questions - these are optional, substantially harder, and a maximum of 16% is available for doing them.  

# Challenge question A
Challenge_A <- function() {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  #setting initial parameters
  s_rich <- c()
  s_rich_2 <- c()
  community <- init_community_max(100)
  community_2 <- init_community_min(100)
  speciation_rate <- 0.1 #same as in question 16()
  s_rich <- c(species_richness(community),s_rich)
  s_rich_2 <- c(species_richness(community_2),s_rich_2)
  for (i in 1:200){
    s_rich <- sum_vect(s_rich,neutral_time_series_speciation(community,speciation_rate,200))
    s_rich_2 <- sum_vect(s_rich_2,neutral_time_series_speciation(community_2,speciation_rate,200))
  }
  #getting mean species richness for the 200 iterations and median for point
  dem <- rep(200, length(s_rich))
  final_s_rich <- s_rich/dem
  final_s_rich_2 <- s_rich_2/dem
  mean_final_s_rich <- mean(final_s_rich)
  mean_final_s_rich_2 <- mean(final_s_rich_2)
  bound <- matrix(nrow = length(final_s_rich), ncol = 2) #for CI
  bound_2 <- matrix(nrow = length(final_s_rich_2), ncol = 2)
  tot_vect <- c(final_s_rich,final_s_rich_2) #combine to get media
  med_value <- median(tot_vect)
  #plotting median
  x_val <- c()
  count <- 0
  for (k in 1:length(final_s_rich)){
    if (final_s_rich[k] >= med_value - (med_value * 0.0005) && final_s_rich[k] <= med_value * 1.0005){
      count <- count + 1 #creates a range for the value to fall within median value 
      x_val <- k
      print(x_val)
      if (count == 1){
        break
      }
    }
  }
  #Confidence intervals
  stdev <- sd(final_s_rich)#standard deviations of means
  #sd_final_s_rich_2 <- final_s_rich_2[c(-1:-6)] 
  stdev_2 <- sd(final_s_rich_2)
  n <- length(final_s_rich)
  CI <- (2.82 * stdev)/sqrt(n) #97.2% CI
  CI_2 <- (2.82 * stdev_2)/sqrt(n) #97.2% CI
  for (j in 1:length(final_s_rich)){
    bound[j,1] <- final_s_rich[j] - CI
    bound[j,2] <- final_s_rich[j] + CI
    bound_2[j,1] <- final_s_rich_2[j] - CI_2
    bound_2[j,2] <- final_s_rich_2[j] + CI_2
  }
  #plotting
  time <- seq(length(final_s_rich))
  par(mfrow=c(1,1))
  plot(time, final_s_rich, col="red", main="Species Richness with Confidence Intervals", cex.main=1.5, ylim = c(0,100),type="l", ylab = "Mean species richness", xlab = "Generations")
  lines(time, final_s_rich_2, col="blue")
  polygon(c(time,rev(time)),c(bound[,1],rev(bound[,2])),col = rgb(1,0.2,0.2,0.8), border = FALSE)
  polygon(c(time,rev(time)),c(bound_2[,1],rev(bound_2[,2])),col = rgb(0.2,0.4,1,0.2), border = FALSE)
  points.default(x=x_val, y=med_value, type='p',pch = 4,cex = 2)
  legend(x = 138.1, y = 95.35, legend = c("Initialise Max", "Initialise Min","Dynamic Equilibrium"), col = c("red","blue","black"), lty=c(1,1,1),cex=0.8)
}

# Challenge question B
Challenge_B <- function() {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  c <- 1
  start_size <- 100
  print(paste("Loop", c))
  s_rich <- c() #vector for storing species richness
  speciation_rate <- 0.003874
  community <- sample(1:start_size,start_size,replace = TRUE)
  s_rich <- c(species_richness(community),s_rich)
  print(paste("Beginning species richness is", s_rich))
  for (j in 1:200){
    s_rich <- sum_vect(s_rich,neutral_time_series_speciation(community,speciation_rate,200))
  }
  dem <- rep(200, length(s_rich))
  final_s_rich <- s_rich/dem
  time <- seq(length(final_s_rich))
  par(mfrow=c(1,1))
  plot(time, final_s_rich, col="purple", main="Mean Species Richness for different Initial Communities",cex.main=1.2, type="l", ylab = "Mean species richness", xlab = "Generations")
  #repeat with many different inital states
  count = 1
  colors <- seq(1,8,1) #generate a colour for each plot
  for (k in 2:8){
    rand <- runif(1,0,1)
    if (rand <=0.5){
      size <- c(1:50)
    } else{
      size <-c(51:100)
    }
    count = count + 1 
    print(paste("Loop",count))
    s_rich_after <- c() #for the different communities after the first 
    community_after <- sample(size,length(size)*2,replace = TRUE)
    s_rich_after <- c(species_richness(community_after),s_rich_after)
    print(paste("Beginning Species Richness is",s_rich_after))
    for (m in 1:200){
      s_rich_after <- sum_vect(s_rich_after,neutral_time_series_speciation(community_after,speciation_rate,200))
    }
    final_s_rich_after <- s_rich_after/dem
    lines(time,final_s_rich_after,col=colors[k])
  }
}


# Challenge question C
Challenge_C <- function() {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  species_sum_500 <- c(0,0)
  species_sum_1000 <- c(0,0)
  species_sum_2500 <- c(0,0)
  species_sum_5000 <- c(0,0)
  for (i in 1:100){
    load(paste0("RDA_results/Cluster_run_iteration_",i,"_.rda")) #loads the cluster results
    if (i %% 4 == 1){ #for size 500
      species_sum_500 <- sum_vect(species_sum_500,time_series) #sum time series into a vector
    } 
    if (i %% 4 == 2){ #for size 1000
      species_sum_1000 <- sum_vect(species_sum_1000,time_series) #sum time series into a vector
    } 
    if (i %% 4 == 3){ #for size 2500
      species_sum_2500 <- sum_vect(species_sum_2500,time_series) #sum time series into a vector
    } 
    if (i %% 4 == 0){ #for size 5000
      species_sum_5000 <- sum_vect(species_sum_5000,time_series) #sum time series into a vector
    }
  }
  time_length_500  <- seq(length(species_sum_500))
  time_length_1000 <- seq(length(species_sum_1000))
  time_length_2500 <- seq(length(species_sum_2500))
  time_length_5000 <- seq(length(species_sum_5000))
  average_species_sum_500 <- species_sum_500/(8*500) #interval_rich == 1 and was recorded for size * 8 times
  average_species_sum_1000 <- species_sum_1000/(8*1000)
  average_species_sum_2500 <- species_sum_2500/(8*2500)
  average_species_sum_5000 <- species_sum_5000/(8*5000)
  plot(time_length_500,ylim = c(0.05,0.08),average_species_sum_500, type = "l", col = "red", xlab = "Generations", ylab = "Mean Species Richness", main = "Average Species Richness of Cluster Run")
  lines(time_length_1000, average_species_sum_1000, type = "l", col = "blue")
  lines(time_length_2500, average_species_sum_2500, type = "l", col = "green")
  lines(time_length_5000, average_species_sum_5000, type = "l", col = "black")
  legend(x = 0, y = 0.056, legend = c("Size 500", "Size 1000", "Size 2500", "Size 5000"), col = c("red","blue","green", "black"),lty=1:2, cex=0.8)
  return()
}

# Challenge question D
Challenge_D <- function(J=500, v=0.003874) {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  Coalescence <- function(){
    theta <- (v * ((J-1)/(1-v)))
    octave_sum <- c(0,0)
    for (i in 1:200){
      lineages <- init_community_min(J) #vector of length j of 1's
      abundances <- c()
      N <- J
      while (N > 1) {
        j <- sample(1:length(lineages), 1) #randomly chooses an index
        randnum <- runif(1,0,1) #random decimal
        condition <- (theta / (theta + N - 1))
        if (randnum < condition){
          abundances <- c(abundances,lineages[j])
        }else{
          index_i <- sample(1:length(lineages), 1) #choose a new random index
          while (index_i == j){
            index_i <- sample(1:length(lineages), 1) #choose a new random index if equal to j
          }
          lineages[index_i] <- lineages[index_i] + lineages[j]
        }
        lineages <- lineages[-j]
        N <- N-1
      }
      abundances <- c(abundances,lineages)
      octave_sum <- sum_vect(octave_sum,octaves(abundances)) 
    }
    octave_avg <- octave_sum/200
    barplot(octave_avg, xlab="Octaves", cex.names=0.8, ylab="Average Species Abundance", main="Coalescence Model for Size 500", col = "grey")
  }
  x<-(system.time(Coalescence()))
  print(x)
  y <- ((2505470/200) * x[3])/3600
  return(paste("We can see that the coalescence model produces similar results to that of the cluster run. From the system.time, we can see that the time was",round(x[3], digits = 2),"seconds. This model ran for 200 iterations. In the cluster run, the average generations for size 500 was 2,505,470, in 12 CPU hours. If we were to run the coalescence model for the same number of iterations, it would take, ((2505470/200) *",round(x[3],digits = 2),")/3600 =" ,round(y, digits = 2),". The reason it is faster is because the coalescence model doesn't need a burn in period, as it works in reverse, starting from the final point and working backwards."))
}

# Challenge question E
Challenge_E <- function() {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  par(mfrow = c(1,1))
  loc <- matrix(nrow = 4, ncol = 2, byrow = TRUE)
  row.names(loc) = c("A","B","C","X")
  colnames(loc)=c('x','y')
  loc[1,] <- c(0,0)
  loc[2,] <- c(3,4)
  loc[3,] <- c(4,1)
  loc[4,] <- c(2,1.5)
  plot(x=loc[,1],y=loc[,2])
  points.default(x=loc[4,1],y=loc[4,2],type='p',pch = 4,cex = 0.5)
  A <- c(0,0)
  B <- c(3,4)
  C <- c(4,1)
  X <- c(2,1.5)
  for (j in 1:20){
    prob <- runif(1,min = 0,max = 1)
    vec <- c()
    if (prob <= 1/3){
      vec <- A
    }else if (prob >=2/3){
      vec <- B
    }else{
      vec <- C
    }
    X[1] <- (X[1] + vec[1])/2
    X[2] <- (X[2] + vec[2])/2
    points.default(x = X[1], y = X[2],type = 'p',pch = 4, cex = 1.5,col = "red")
  }
  for (i in 1:50000){
    prob <- runif(1,min = 0,max = 1)
    vec <- c()
    if (prob <= 1/3){
      vec <- A
    }else if (prob >=2/3){
      vec <- B
    }else{
      vec <- C
    }
    X[1] <- (X[1] + vec[1])/2
    X[2] <- (X[2] + vec[2])/2
    points.default(x = X[1], y = X[2],type = 'p',pch = 4, cex = 0.08)
  }
  return("The fractal formed is still Sierpinki's gasket with size of 3 and length of 2. Even if the starting point is not in (0,0), since there are still the same 3 points to which vector X can move to, X slowly moves towards Sierpinki's gasket. This can be seen with the red marks.")
}

# Challenge question F
Challenge_F <- function() {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  par(mfrow=c(2,2))
  colors <- c("red","green","blue","black")
  x <- c(0.0001,0.005,0.01, 0.05) #different values for line size threshold
  for (i in 1:4){
    turtle <- function(start_position, direction, length, colors)  {
      end_position1 <- length * cos(direction)
      end_position2 <- length * sin(direction)
      segments(start_position[1],start_position[2], start_position[1]+end_position1,start_position[2]+end_position2, col = colors)
      return(c(start_position[1]+end_position1,start_position[2]+end_position2)) # you should return your endpoint here.
    }
    fern2 <- function(start_position, dir, direction, length) {
      new_start <- turtle(start_position, direction, length, colors[i])
      dir <- dir * -1
      new_direction <- direction + dir * (pi/4) #moves to the left 
      new_length <- length * 0.38 #changing line size threshold
      other_length <- length * 0.77
      other_direction <- direction 
      if (new_length>=x[i]){
        fern2(new_start,-dir,new_direction,new_length)
        fern2(new_start,dir,other_direction,other_length)
      } 
    }
    draw_fern2 <- function()  {
      plot(0:6,0:6,type='n',main = paste("Line threshold of:",x[i]), cex.main=0.8)
      dir <- -1
      fern2(c(1,0),dir,pi/4,1.6) 
    }
    draw_fern2()
  }
  return("Differentiating the line size threshold leads to different amounts of detail being produced.The smaller the threshold, the more lines are plotted thus producing a more detailed version of the fern, as seen in the thickness of the leaves.") 
}
# Challenge question G should be written in a separate file that has no dependencies on any functions here.