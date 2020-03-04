#################################################
# BASIC GENE DRIVE SIMULATOR, PURE R CODE
# CMEE MSc 2020 TIN-YU HUI
# DRIFT: WRIGHT-FISHER MODEL
# SELECTION: 11 (HEG) HOMOZYGOTE ARE LETHAL
# DRIVE: SUPER MENDELIAN INHERITANCE OF THE HEG ALLELE (FROM HETEROZYGOTES) WITH RATIO d
#################################################

# INPUT PARAMETERS
# q0: RELEASING FREQ OF HEG
# N0: INITIAL POPULATION SIZE
# d: HOMING RATE OF HEG (d>0.5)
# t: NUMBER OF GENERATIONS 
# R0, M: THE TWO PARAMETERS FOR BEVERTON-HOLT POPULATION MODEL- growth rate and (R0-1)*M is carrying capacity
# M controls competition where M defines # of individuals in the pop. where surving competition is a half
gene_drive<-function(q0=0.05, d=0.6, t=10, N0=500, R0=2, M=500)
{
  # SOME CHECKS ON THE INPUT PARAMETERS
  if (q0<=0 || q0>0.5)
  {stop('q HAS TO BE BETWEEN 0 AND 0.5')}
  if (d<=0.5 || d>=1)
  {stop('d HAS TO BE BETWEEN 0.5 AND 1')}
  
  # BEVERTON-HOLT MODEL FOR POPULATION DYNAMICS for N(t+1)
  # ceiling() TO ROUND IT UP 
  bh<-function(N, R0, M)
  {return(ceiling(R0*N/(1+N/M)))}
  
  # INITIALISE
  # CREATE SOME EMPTY LIST AND VECTORS TO STORE THE REQUIRED INFORMATION
  population<-list()
  length(population)<-(t+1)
  # OPTIONAL STEP, TO GIVE NAMES TO EVERY ELEMENTS OF population
  for (i in 1:length(population))
  {names(population)[i]<-paste(c('generation', i-1), collapse='')}
  population.size<-rep(NA, t+1)
  allele.freq.q<-rep(NA, t+1)
  
  # FILL IN THE INITIAL VALUES
  population.size[1]<-N0
  allele.freq.q[1]<-q0
  
  # WE WILL RELEASE k TRANSGENIC MOSQUITOES, AND THEY ARE 01 HETEROZYGOTE
  # THE INITIAL POPULATION HAS k TRANSGENIC MOSQUITOES AND (N0-k) WILDTYPE MOSQUITOES WITH 00 ALLELE
  # THERE'S NO NEED TO SHUFFLE THEM
  k<-ceiling(2*N0*q0)
  population[[1]]<-matrix(c(rep(c(0,0), N0-k), rep(c(0,1), k)), nr=2)
  # ALSO RECORD THE COUNTS FOR GENOTYPES 00, 01, 11
  genotype<-c(N0-k, k, 0)
  
  # PROPAGATION
  for (i in 1:t)
  {
    # CALCULATE THE NEW POPULATION SIZE USING bh(). ONLY genotype[1]+genotype[2] SURVIVE TILL ADULTHOOD. 
    population.size[i+1]<-bh(genotype[1]+genotype[2], R0, M)
    # EARLY EXIT CONDITION, IF THE POPULATION SIZE DROP TO 1
    if (population.size[i+1]<=1)
    {
      print(paste(c('Oops! The population crashed after generation ', i-1), collapse=''))
      return(list(population=population[1:i], population.size=population.size[1:i], allele.freq.q=allele.freq.q[1:i]))
    }
    # EARLY EXIT CONDITION, IF HEG WENT EXTINCT
    if (genotype[2]+genotype[3]==0)
    {
      print(paste(c('Oops! HEG went extinct at generation ', i-1), collapse=''))
      return(list(population=population[1:i], population.size=population.size[1:i], allele.freq.q=allele.freq.q[1:i]))
    }
    # IF NEITHER OF THE TWO OCCURED. WE CAN PROCEED
    # THE NEW GAMETE FREQ OF HEG
    gamete.freq.q<-genotype[2]*d/(genotype[1]+genotype[2])
    # FILL IN THE NEW POPULATION. UPDATA THE GENOTYPE COUNTS AND ALLELE FREQ
    population[[i+1]]<-matrix(sample(0:1, size=2*population.size[i+1], prob=c(1-gamete.freq.q, gamete.freq.q), replace=T), nr=2)
    temp<-apply(population[[i+1]], 2, sum)
    genotype<-c(sum(temp==0), sum(temp==1), sum(temp==2))
    allele.freq.q[i+1]<-(0.5*genotype[2]+genotype[3])/population.size[i+1]
  }
  # OUTPUT. RETURN A BIG LIST
  return(list(population=population, population.size=population.size, allele.freq.q=allele.freq.q))
}

# TEST RUN
gene_drive(q0=0.05, d=0.6, t=10)

#Problem 1
t=50
temp1= gene_drive(q0=0.05, d=0.6, t=50,N=500, M=500, R0=2)
plot(0:t)

#Problem 2- proportion that survive (1 - #extinct)/# of trials --> prob. of surviving
#use different q0 values to see possible minimum starting frequency

#Problem 3- vary the value of d by increasing it

#Problem 4- change the inrtinisc growth rate and starting population values
