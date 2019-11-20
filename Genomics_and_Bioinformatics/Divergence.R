rm(list=ls())
western_band_gecko <- read.csv("genomics_and_bioinformatics/Practicals/western_banded_gecko.csv", header=FALSE, stringsAsFactors = FALSE, colClasses = c("character"))
western_band_gecko[1:10,1:10]
bent_toed_gecko <- read.csv("genomics_and_bioinformatics/Practicals/bent-toed_gecko.csv", header=FALSE, stringsAsFactors = FALSE, colClasses = c("character"))
bent_toed_gecko[1:10,1:10]
leopard_gecko <- read.csv("genomics_and_bioinformatics/Practicals/leopard_gecko.csv", header=FALSE, stringsAsFactors = FALSE, colClasses = c("character"))
leopard_gecko[1:10,1:10]

dim(bent_toed_gecko) #20,20,000

merge_data <- rbind(bent_toed_gecko,western_band_gecko)
dim(merge_data) #40,200000

not_fix_bt_col <- c() #find polymorphic sites of bt
for (i in 1:ncol(bent_toed_gecko)){
    if (length(unique(bent_toed_gecko[ ,i]))>1){
      not_fix_bt_col <- c(not_fix_bt_col,i)
    }
}
length(not_fix_bt_col) #62

not_fix_wb_col <- c() #find polymorphic sites of wb
for (j in 1:ncol(western_band_gecko)){
  if (length(unique(western_band_gecko[ ,j]))>1){
    not_fix_wb_col <- c(not_fix_wb_col,j)
  }
}
length(not_fix_wb_col) #58
58+62 #120 is the total number of polymorphic sites to remove

poly <- unique(c(not_fix_bt_col,not_fix_wb_col))
length(poly) #120 

dim(merge_data) #40,20000
#removes all polymorphic sites from merged data
merge_data <- merge_data[ ,-poly] #if you use for loop, work from last to first as the data shifts for every deletion
dim(merge_data) #40,19880

count <- 0 #counts the different sites of the fixed sites between species
for (v in 1:ncol(merge_data)){
  if (length(unique(merge_data[ ,v]))>1){
    count <- count + 1
  }
}
count #73

genetic_divergence <- count/ncol(merge_data)

#find the mutation rate
l_bt_merge <- rbind(leopard_gecko,bent_toed_gecko)
not_fix_l_col <- c() #find polymorphic sites of leopard
for (r in 1:ncol(leopard_gecko)){
  if (length(unique(leopard_gecko[ ,r]))>1){
    not_fix_l_col <- c(not_fix_l_col,r)
  }
}
length(not_fix_l_col) #47

sites <- unique(c(not_fix_l_col,not_fix_bt_col))
length(sites) #109

l_bt_merge <- l_bt_merge[ ,-sites]
dim(l_bt_merge) #40,19891

diff <- 0
for (p in 1:ncol(l_bt_merge)){
  if (length(unique(l_bt_merge[ ,p]))>1){
    diff <- diff + 1
  }
}
diff #181
genetic_diff <- diff/ncol(l_bt_merge)

mutation_rate <- genetic_diff/(2*30000000)

#time divergence between bent_toed and western_banded
time_diverge <- genetic_divergence/(2*mutation_rate)

#Bonus question
divergence <- (2*mutation_rate*(30000000-time_diverge)) #0.005427561










  
