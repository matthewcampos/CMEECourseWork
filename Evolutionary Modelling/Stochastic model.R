#initialise matrix of population and allele 0 frequency
sim.gene.drive = function(q0, N, t){
  q_freq = vector(mode = "numeric", length = t + 1)
  q_freq[1] = q0
  population = list()
  k = ceiling(2*N*q0)
  
  # Populate first matrix with starting configuration
  population[[1]] = matrix(sample(c(rep(0,k), rep(1, 2*N-k))), nr = 2)
  
  for(i in 1:t){
    population[[i+1]] = matrix(sample(0:1, size = 2*N, prob = c(q_freq[i], 1-q_freq[i]), replace = T), nr = 2)
    
    #allele freq at next gen = 
    q_freq[i+1] = sum(population[[i+1]] == 0)/(2*N)
  }
  # Put all outputs into another list and then return them
  return(list(population = population, q_freq = q_freq))
}
sim.gene.drive(q0 = 0.5, N = 200, t = 10)
final_q_freq <- c()
#mean p0
for (i in 1:1000){
  x<-sim.gene.drive(q0 = 0.5, N = 200, t = 10)
  final_q_freq <- c(x$q_freq[length(x$q_freq)],final_q_freq)
  mean_p0 <- mean(final_q_freq)
  var_p0 <- var(final_q_freq)
}
print(mean_p0)
print(var_p0)
#mean persistence time
sim.timegene.drive = function(q0, N){
  q_freq = q0
  population = list()
  k = ceiling(2*N*q0)
  # Populate first matrix with starting configuration
  population = matrix(sample(c(rep(0,k), rep(1, 2*N-k))), nr = 2)
  t <- 0
  while(q_freq>0 & q_freq < 1){
    population = matrix(sample(c(0,1), size = 2*N, prob = c(q_freq, 1-q_freq), replace = T), nr = 2)
    #allele freq at next gen = 
    q_freq = sum(population == 0)/(2*N)
    t<- t + 1
  }
  # Put all outputs into another list and then return them
  return(t)
}

sim.timegene.drive(q0 = 0.1, N = 50)

time <- c()
for (i in 1:1000){
  x<-sim.timegene.drive(q0 = 0.1, N = 50)
  time <- c(x,time)
}
mean(time)

#haplotype.shinyapps.io/gene_drive



