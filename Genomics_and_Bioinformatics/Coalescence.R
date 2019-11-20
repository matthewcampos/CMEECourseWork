rm(list=ls())
ks_north <- read.csv("genomics_and_bioinformatics/Practicals/killer_whale_North.csv", header=FALSE, stringsAsFactors = FALSE, colClasses = c("character"))
ks_north[1:10,1:10]
ks_south <- read.csv("genomics_and_bioinformatics/Practicals/killer_whale_South.csv", header=FALSE, stringsAsFactors = FALSE, colClasses = c("character"))
ks_south[1:10,1:10]             

#remove columns with only 1 type of character
n_col <- c()
for (i in 1:50000){ 
    if (length(unique(ks_north[ ,i]))==1){
      n_col <- c(n_col,i)
    }
}
ks_north <- ks_north[ ,-n_col]

s_col <- c()
for (j in 1:ncol(ks_south)){
    if (length(unique(ks_south[ ,j]))==1){
      s_col <- c(s_col,j)
    }
}
ks_south <- ks_south[ ,-s_col]

#tajima method
north_counter <- 0
for (q in 1:(nrow(ks_north)-1)){
  for (p in (q+1):nrow(ks_north)){
    for (m in 1:ncol(ks_north)){
      if (ks_north[k,m]!=ks_north[p,m]){
        north_counter <- north_counter + 1
      }
    }
  }
}
north_counter
tajima_north <- north_counter/((nrow(ks_north)*(nrow(ks_north)-1))/2)

south_counter <- 0
for (l in 1:(nrow(ks_south)-1)){
  for (k in (l+1):nrow(ks_south)){
    for (m in 1:ncol(ks_south)){
      if (ks_south[l,m]!=ks_south[k,m]){
        south_counter <- south_counter + 1
      }
    }
  }
}
south_counter
tajima_south <- south_counter/((nrow(ks_south)*(nrow(ks_south)-1))/2)


#can use dist function to compare pairwise (manhattan method)

#watterson method
r_n <- 0
for (z in 1:ncol(ks_north)){
  r_n <- r_n+1 #number of segregrating sites
}
tot_n <- c()
for (x in 1:(nrow(ks_north)-1)){
  tot_n <- c(tot_n,x)
}
tot_n <- sum(1/tot_n)
watterson_theta_north <- r_n/tot_n

r_s <- 0
for (y in 1:ncol(ks_south)){
  r_s <- r_s+1 #number of segregrating sites
}
tot_s <- c()
for (u in 1:(nrow(ks_south)-1)){
  tot_s <- c(tot_s,u)
}
tot_s <- sum(1/tot_s)
watterson_theta_south <- r_s/tot_s

denominator <- 4 * (1*10^(-8)) * 50000
tajima_ne_north <- tajima_north/(denominator)
print(tajima_ne_north)
tajima_ne_south <- tajima_south/(denominator)
print(tajima_ne_south)

dem <- 4 * (1*10^(-8)) * 50000
watterson_ne_north <- watterson_theta_north/(dem)
print(watterson_ne_north)
watterson_ne_south <- watterson_theta_south/(dem)
print(watterson_ne_south)

#Site Frequency
south <- matrix(NA, nrow = 1, ncol = ncol(ks_south), byrow = TRUE)
north <- matrix(NA, nrow = 1, ncol = ncol(ks_north), byrow = TRUE)

for (h in 1:ncol(ks_south)){
  south_count <- 0
  for (g in 1:nrow(ks_south)){
    if (ks_south[g,h]==1){
      south_count <- south_count + 1
      south[,h] <- south_count
    }
  }
}

for (h_x in 1:ncol(ks_north)){
  north_count <- 0
  for (g_x in 1:nrow(ks_south)){
    if (ks_north[g_x,h_x]==1){
      north_count <- north_count + 1
      north[,h_x] <- north_count
    }
  }
}




