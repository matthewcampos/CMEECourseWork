rm(list=ls())
data <- read.csv("genomics_and_bioinformatics/practicals/bears.csv", header=FALSE, stringsAsFactors = FALSE, colClasses = c("character"))
dim(data)
sample <- data[1:10,1:100]

#1 identify the positions that are SNP's
SNP <- function(data){
  counter<-0 #represents each column 
  location <- c() #show the column and number of unique items
  for (i in 1:ncol(data)){
    counter <- counter+1 #each loop it adds one to show which column it is in
    x <- length(unique(data[ ,i])) #length identifies how many distinct items there are each column
    if (x > 1){
      location <- c(location,counter) #prints the column and number of unique items in each column
    }
  }
  return(location)
}


#2 calculate, print and visualise allele frequencies for each SNP
allele_frequency <- function(data){
  frequencies <- c()
  for (q in location){
    alleles= unique(data[ ,q]) #shows the unique bases to each column
    frequency_1 <- length(which(data[ ,q]==alleles[1]))/nrow(data) #which is similar to if, and length counts how many times condition is met
    frequency_2 <- 1-frequency_1
    #print(c(alleles[1],frequency_1))
    #print(c(alleles[2],frequency_2))
    frequencies <- c(frequencies,frequency_1,frequency_2)
  }
  return(frequencies)
}

#3 calculate and print genotype frequencies for each SNP
observed_genotype_frequencies_homo1 <- c()
observed_genotype_frequencies_homo2 <- c()
observed_genotype_frequencies_het <- c()
for (q in location){
  alleles= unique(data[ ,q])
  genotypes <- c(0,0,0)
  for (x in 1:(nrow(data)/2)){
    col_genotype <- matrix(NA, nrow=10, ncol=2, byrow=TRUE)
    individual_1 <- data[x*2,q]
    individual_2 <- data[(x*2)-1,q]
    col_genotype <- c(individual_2,individual_1) #pairs the alleles to form genotypes
    count <- length(which(col_genotype==alleles[2])) #counts for homozygote 1, homozygote 2, heterozygote
    genotypes[count+1]=genotypes[count+1]+1 #assign count to genotypes
  }
  print(genotypes)
  freq_hom_1 <- genotypes[1]/(nrow(data)/2)
  freq_hom_2 <- genotypes[3]/(nrow(data)/2)
  freq_het <- genotypes[2]/(nrow(data)/2)
  observed_genotype_frequencies_homo1 <- c(observed_genotype_frequencies_homo1,freq_hom_1)
  observed_genotype_frequencies_homo2 <- c(observed_genotype_frequencies_homo2,freq_hom_2)
  observed_genotype_frequencies_het <- c(observed_genotype_frequencies_het, freq_het)
  #genotype_frequencies <- c(genotype_frequencies,c(freq_hom_1,freq_hom_2,freq_het))
  }

#4 calculate (observed) homozygosity and heterozygosity for each SNP
observed_genotypes <- c()
for (q in location){
  alleles= unique(data[ ,q])
  genotypes <- c(0,0,0)
  for (x in 1:(nrow(data)/2)){
    col_genotype <- matrix(NA, nrow=10, ncol=2, byrow=TRUE)
    individual_1 <- data[x*2,q]
    individual_2 <- data[(x*2)-1,q]
    col_genotype <- c(individual_2,individual_1)
    count <- length(which(col_genotype==alleles[2])) #checks for homozygote 1, homozygote 2, heterozygote
    genotypes[count+1]=genotypes[count+1]+1 #assign count to genotypes
  }
  freq_hom_1 <- genotypes[1]/(nrow(data)/2)
  freq_hom_2 <- genotypes[3]/(nrow(data)/2)
  freq_het <- genotypes[2]/(nrow(data)/2)
  freq_hom <- freq_hom_1 + freq_hom_2
  freq_h <- 1-freq_hom
  observed_genotypes <- c(observed_genotypes,c(freq_hom,freq_h))
}

#5 calculate expected genotype counts for each SNP and test for HWE
expected_genotype_homo1 <- c()
expected_genotype_homo2 <- c()
expected_genotype_het <- c()
for (q in location){
  alleles= unique(data[ ,q]) #shows the unique bases to each column
  frequency_1 <- length(which(data[ ,q]==alleles[1]))/nrow(data) #which is similar to if, and length counts how many times condition is met
  frequency_2 <- length(which(data[ ,q]==alleles[2]))/nrow(data)
  expected_hetero <- 2*frequency_1*frequency_2
  expected_homo1 <- frequency_1*frequency_1
  expected_homo2 <- frequency_2*frequency_2
  expected_genotype_homo1 <- c(expected_genotype_homo1,expected_homo1)
  expected_genotype_homo2 <- c(expected_genotype_homo2,expected_homo2)
  expected_genotype_het <- c(expected_genotype_het,expected_hetero)
}
#Chi-squared test
chi_homo_1 <- (expected_genotype_homo1-observed_genotype_frequencies_homo1)^2/expected_genotype_homo1
chi_homo_2 <- (expected_genotype_homo2-observed_genotype_frequencies_homo2)^2/expected_genotype_homo2
chi_hetero <- (expected_genotype_het-observed_genotype_frequencies_het)^2/expected_genotype_het
chi <- chi_homo_1+chi_homo_2+chi_hetero
p_value <- 1-pchisq(chi, df=1)

#6 calculate, print and visualise inbreeding coefficient for each SNP deviating from HWE
F_value <- function(data){
  f <-(expected_genotype_het - observed_genotype_frequencies_het)/expected_genotype_het
  return(f)
}
















