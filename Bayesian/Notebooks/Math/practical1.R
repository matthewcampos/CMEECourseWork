source("../Math/Data/functions.R")
#A
data <- "AAGAGGA"
error_rate=0.05
#function for likelihood
calcGenoLikes("AAGAGGA", "A", "G", error_rate, FALSE)
#function for prior
prior <- function(allele_1,allele_2){
  prior <- c(1/3,1/3,1/3)
  names(prior) <- c(paste0(allele_1,allele_1),paste0(allele_1,allele_2),paste0(allele_2,allele_2))
  return(prior)
}
#Posterior distribution
prob_G_given_D <- function(allele_1,allele_2){
  posterior <- c()
  x <- calcGenoLikes(data, "A", "G", error_rate, FALSE)
  likelihood_value <- c() #likelihood value desired
  if (allele_1 == "A" & allele_2 =="A"){
    likelihood_value <- x[1]
  }else if (allele_1 == "G" & allele_2 == "G"){
    likelihood_value <- x[3]
  }else{
    likelihood_value <- x[2]
  }
  posterior <- (likelihood_value * prior(allele_1,allele_2)[1]) / sum(x*prior(allele_1,allele_2))
  return(posterior)
}
prob_G_given_D("A","A")

#B
data <- "AAAG"
error_rate = 0.01
BAA=prob_G_given_D("A","A")
BAG=prob_G_given_D("A","G")
BGG=prob_G_given_D("G","G")

#C
#HWE where f(G) = 0.1 thus f(A) = 0.9
#GG=0.1^2, AA=0.9^2 and AG=2(0.1)(0.9)
data <- "AAAG"
error_rate = 0.01
prior <- function(allele_1,allele_2){
  prior <- c(0.9^2,2*(0.1)*(0.9),(0.1)^2)
  names(prior) <- c(paste0(allele_1,allele_1),paste0(allele_1,allele_2),paste0(allele_2,allele_2))
  return(prior)
}
#Posterior distribution
prob_G_given_D <- function(allele_1,allele_2){
  likelihood <- calcGenoLikes(data, "A", "G", error_rate, FALSE)
  if (allele_1 == "A" & allele_2 =="A"){
    likelihood_value <- likelihood[1]
  }else if (allele_1 == "G" & allele_2 == "G"){
    likelihood_value <- likelihood[3]
  }else{
    likelihood_value <- likelihood[2]
  }
  y <- prior(allele_1,allele_2)
  if (allele_1 == "A" & allele_2 =="A"){
    prior_value <- y[1]
  }else if (allele_1 == "G" & allele_2 == "G"){
    prior_value <- y[3]
  }else{
    prior_value <- y[2]
  }
  posterior <- (likelihood_value * prior_value) / sum(likelihood*prior(allele_1,allele_2))
  return(posterior)
}
CAA=(prob_G_given_D("A","A"))
CAG=(prob_G_given_D("A","G"))
CGG=(prob_G_given_D("G","G"))
sum(c(prob_G_given_D("A","A")),prob_G_given_D("A","G"),prob_G_given_D("G","G"))

#D
#HWE where f(G) = 0.1 thus f(A) = 0.9
#GG=0.1^2, AA=0.9^2 and AG=2(0.1)(0.9)
data <- "AAAG"
error_rate = 0.01
prior <- function(allele_1,allele_2){
  prior <- c((0.9^2 + 0.2 * 0.9 * 0.1),(2*(0.1)*(0.9) * (1-0.2)),((0.1)^2) + 0.2 * 0.9 * 0.1)
  names(prior) <- c(paste0(allele_1,allele_1),paste0(allele_1,allele_2),paste0(allele_2,allele_2))
  return(prior)
}
#Posterior distribution
prob_G_given_D <- function(allele_1,allele_2){
  likelihood <- calcGenoLikes(data, "A", "G", error_rate, FALSE)
  if (allele_1 == "A" & allele_2 =="A"){
    likelihood_value <- likelihood[1]
  }else if (allele_1 == "G" & allele_2 == "G"){
    likelihood_value <- likelihood[3]
  }else{
    likelihood_value <- likelihood[2]
  }
  y <- prior(allele_1,allele_2)
  if (allele_1 == "A" & allele_2 =="A"){
    prior_value <- y[1]
  }else if (allele_1 == "G" & allele_2 == "G"){
    prior_value <- y[3]
  }else{
    prior_value <- y[2]
  }
  posterior <- (likelihood_value * prior_value) / sum(likelihood*prior(allele_1,allele_2))
  return(posterior)
}
DAA=(prob_G_given_D("A","A"))
DAG=(prob_G_given_D("A","G"))
DGG=(prob_G_given_D("G","G"))

#E
#HWE where f(G) = 0.1 thus f(A) = 0.9
#GG=0.1^2, AA=0.9^2 and AG=2(0.1)(0.9)
data <- "AAAG"
error_rate = 0.05
prior <- function(allele_1,allele_2){
  prior <- c((0.9^2 + 0.2 * 0.9 * 0.1),(2*(0.1)*(0.9) * (1-0.2)),((0.1)^2) + 0.2 * 0.9 * 0.1)
  names(prior) <- c(paste0(allele_1,allele_1),paste0(allele_1,allele_2),paste0(allele_2,allele_2))
  return(prior)
}
#Posterior distribution
prob_G_given_D <- function(allele_1,allele_2){
  likelihood <- calcGenoLikes(data, "A", "G", error_rate, FALSE)
  if (allele_1 == "A" & allele_2 =="A"){
    likelihood_value <- likelihood[1]
  }else if (allele_1 == "G" & allele_2 == "G"){
    likelihood_value <- likelihood[3]
  }else{
    likelihood_value <- likelihood[2]
  }
  y <- prior(allele_1,allele_2)
  if (allele_1 == "A" & allele_2 =="A"){
    prior_value <- y[1]
  }else if (allele_1 == "G" & allele_2 == "G"){
    prior_value <- y[3]
  }else{
    prior_value <- y[2]
  }
  posterior <- (likelihood_value * prior_value) / sum(likelihood*prior(allele_1,allele_2))
  return(posterior)
}
EAA=(prob_G_given_D("A","A"))
EAG=(prob_G_given_D("A","G"))
EGG=(prob_G_given_D("G","G"))

#F
AA <- c(BAA,CAA,DAA,EAA)
AG <- c(BAG,CAG,DAG,EAG)
GG <- c(BGG,CGG,DGG,EGG)
result_matrix <- matrix(c(AA,AG,GG),nrow = 4,ncol = 3)
colnames(result_matrix) <- c("AA","AG","GG")
barplot(result_matrix,beside=TRUE)

#G
data <- "AAAGAGAAAAAAAGGGGGAAAGGA"
error_rate = 0.05
prior <- function(allele_1,allele_2){
  prior <- c(0.9^2,2*(0.1)*(0.9),(0.1)^2)
  names(prior) <- c(paste0(allele_1,allele_1),paste0(allele_1,allele_2),paste0(allele_2,allele_2))
  return(prior)
}
#Posterior distribution
prob_G_given_D <- function(allele_1,allele_2){
  likelihood <- calcGenoLikes(data, "A", "G", error_rate, FALSE)
  if (allele_1 == "A" & allele_2 =="A"){
    likelihood_value <- likelihood[1]
  }else if (allele_1 == "G" & allele_2 == "G"){
    likelihood_value <- likelihood[3]
  }else{
    likelihood_value <- likelihood[2]
  }
  y <- prior(allele_1,allele_2)
  if (allele_1 == "A" & allele_2 =="A"){
    prior_value <- y[1]
  }else if (allele_1 == "G" & allele_2 == "G"){
    prior_value <- y[3]
  }else{
    prior_value <- y[2]
  }
  posterior <- (likelihood_value * prior_value) / sum(likelihood*prior(allele_1,allele_2))
  return(posterior)
}
GAA=(prob_G_given_D("A","A"))
GAG=(prob_G_given_D("A","G"))
GGG=(prob_G_given_D("G","G"))
print(GAA)
print(GAG)
print(GGG)

#H
bases <- paste(c(rep("A",1e3),rep("G",1e3)), sep="", collapse="")
calcGenoLikes(bases, "A", "G", error_rate, TRUE)

