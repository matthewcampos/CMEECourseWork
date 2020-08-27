##Analysis using regression and ANOVA

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

rm(list=ls()) #clear workspace
source('functions.R')
library(dplyr)
library(gridExtra)
library(plotrix)
library(multcompView)
library(agricolae)

#open data
gndiff.50.65.rda_list <- list.files(path = "../Data/GNDIFF_50-65",full.names = TRUE)
gndiff.50.80.rda_list <- list.files(path = "../Data/GNDIFF_50-80",full.names = TRUE)
gnsame.50.65.rda_list <- list.files(path = "../Data/GNSAME_50-65",full.names = TRUE)
gnsame.50.80.rda_list <- list.files(path = "../Data/GNSAME_50-80",full.names = TRUE)
folder.list <- list(gndiff.50.65.rda_list,gndiff.50.80.rda_list,gnsame.50.65.rda_list,gnsame.50.80.rda_list)

#Recovery time of average fitness of runs
#matrix to save each individual run result
maxspeed.recovery.matrix <- matrix(NA,nrow = 880,ncol = 2)
colnames(maxspeed.recovery.matrix) <- c('avg','sd')
for (i in 1:4){
  rda_list <- folder.list[[i]]
  recovery.list <- list()
  recovery.sd.list <-list()
  for (j in 1:44){
    persim.recovery.list <- list()
    persim.recovery.sd.list <-list()
    for (k in 1:5){
      load(paste0(rda_list[j],'/run_1/Fitness/Simulation',k,'.rda'))
      mig_effect <- c()
      recover <- c()
      for (l in 80:700){
        if (j<=5 | j>11 & j<=16| j>22 & j<=27| j>33 & j<=38){
          values <- sort(fit[(l-10):(l-1),1]) #just before migration- every generation
          avg_fit <- mean(values[(length(values)-5):length(values)]) #mean of top five values
          if (fit[l,1] < avg_fit){ #less than avg
            mig_effect <- c(mig_effect,l)
          }else{
            recover <- c(recover,l)
          }
        }else if(j==6|j==8|j==9|j==17|j==19|j==20|j==28|j==30|j==31|j==39|j==41|j==42){
          values <- sort(fit[(l-25):(l-1),1]) #just before migration- every 5 generations
          avg_fit <- mean(values[(length(values)-4):length(values)]) #mean of top five values
          if (fit[l,1] < avg_fit){ #less than avg
            mig_effect <- c(mig_effect,l)
          }else{
            recover <- c(recover,l)
          }
        }else if(j==7|j==10|j==11|j==18|j==21|j==22|j==29|j==32|j==33|j==40|j==43|j==44){
          values <- sort(fit[(l-50):(l-1),1]) #just before migration- every 10 generations
          avg_fit <- mean(values[(length(values)-4):length(values)]) #mean of top five values
          if (fit[l,1] < avg_fit){ #less than avg
            mig_effect <- c(mig_effect,l)
          }else{
            recover <- c(recover,l)
          }
        }
      }
      recovery_time <- c()
      count <- 0 
      if (length(mig_effect)>0){
        for (time in length(mig_effect):2){
          
          if (mig_effect[time] - mig_effect[time - 1] == 1){
            count <- count + 1 #every instance fitness is below avg 
          }else{
            recovery_time <- c(recovery_time,count)
            count <- 0
          }
        }
      }
      if (is.null(recovery_time)==TRUE){
        recovery_time <- c(0,0)
      }
      persim.recovery.list[[length(persim.recovery.list)+1]] <- mean(recovery_time)
      persim.recovery.sd.list[[length(persim.recovery.sd.list)+1]] <- sd(recovery_time)
    }
    recovery.list[[length(recovery.list)+1]] <- persim.recovery.list
    recovery.sd.list[[length(recovery.sd.list)+1]] <- persim.recovery.sd.list
  }
  #saves matrix of the 4 environmental/genetic conditions
  if (i == 1){
    maxspeed.recovery.matrix[1:220,1] <- as.numeric(unlist(persim.recovery.list)) 
    maxspeed.recovery.matrix[1:220,2] <- as.numeric(unlist(persim.recovery.sd.list))
  }else if (i == 2){
    maxspeed.recovery.matrix[221:440,1] <- as.numeric(unlist(persim.recovery.list)) 
    maxspeed.recovery.matrix[221:440,2] <- as.numeric(unlist(persim.recovery.list))
  }else if (i == 3){
    maxspeed.recovery.matrix[441:660,1] <- as.numeric(unlist(persim.recovery.list)) 
    maxspeed.recovery.matrix[441:660,2] <- as.numeric(unlist(persim.recovery.list))
  }else if (i == 4){
    maxspeed.recovery.matrix[661:880,1] <- as.numeric(unlist(persim.recovery.list)) 
    maxspeed.recovery.matrix[661:880,2] <- as.numeric(unlist(persim.recovery.list))
  }
}

#Before and after migration variance ratio for robustness
all.ratio.matrix <- matrix(NA,nrow = 880,ncol = 1)
colnames(all.ratio.matrix) <- 'Ratio'
for (m in 1:4){
  check <- 0
  rda_list <- folder.list[[m]]
  ratio.list <- list()
  for (n in 1:44){
    check <- check + 1
    print(check)
    check.2 <- 0
    ratio.matrix <- matrix(NA,nrow = 5,ncol = 1)
    for (p in 1:5){
      check.2 <-check.2 + 1
      print(check.2)
      load(paste0(rda_list[n],'/run_1/Traits/Simulation',p,'.rda'))
      load(paste0(rda_list[n],'/run_1/Population/Simulation',p,'.rda'))
      #before migration trait values
      #replicates so 4 different mutations per site
      bm.y3 <- yvalues.list[[3]][80] #trait values from gene 3
      bm.y3.max <- which.max(bm.y3[[1]])
      bm.ind <- pop.list[[2]][bm.y3.max,,] #most fit before migration
      bm.pop.1 <- population(size=13,locus=12) #for mutations in 1st strand
      bm.pop.2 <- population(size=13,locus=12) #mutations in 2nd strand
      bm.pop.3 <- population(size=13,locus=12) #repetitions of above
      bm.pop.4 <- population(size=13,locus=12)
      bm.pop.5 <- population(size=13,locus=12) #repetitions of above
      bm.pop.6 <- population(size=13,locus=12)
      bm.pop.7 <- population(size=13,locus=12) #repetitions of above
      bm.pop.8 <- population(size=13,locus=12)
      bm.pop <- population(size=97,locus = 12) #combine both populations to calculate yvalues
      bm.y1.list <- yvalues.list[[1]][80] #allele values from gene 1
      bm.y1.val <- bm.y1.list[[1]][bm.y3.max]
      bm.y2.list <- yvalues.list[[2]][80] #allele values from gene 2
      bm.y2.val <- bm.y2.list[[1]][bm.y3.max]
      #after migration trait values
      am.y3 <- yvalues.list[[3]][710] 
      am.y3.max <- which.max(am.y3[[1]]) #max individual
      am.ind <- pop.list[[13]][am.y3.max,,] #most fit after migration
      am.pop.1 <- population(size=13,locus=12) #for mutations in 1st strand
      am.pop.2 <- population(size=13,locus=12) #mutations in 2nd strand
      am.pop.3 <- population(size=13,locus=12) 
      am.pop.4 <- population(size=13,locus=12) 
      am.pop.5 <- population(size=13,locus=12) 
      am.pop.6 <- population(size=13,locus=12) 
      am.pop.7 <- population(size=13,locus=12) 
      am.pop.8 <- population(size=13,locus=12) 
      am.pop <- population(size=97,locus = 12)
      am.y1.list <- yvalues.list[[1]][710]
      am.y1.val <- am.y1.list[[1]][am.y3.max]
      am.y2.list <- yvalues.list[[2]][710]
      am.y2.val <- am.y2.list[[1]][am.y3.max]
      #replicate individual
      for (person  in 1:dim(am.pop.1)[1]){
        am.pop.1[person,,] <- am.ind #replicates fit individual for mutation
        am.pop.2[person,,] <- am.ind
        am.pop.3[person,,] <- am.ind
        am.pop.4[person,,] <- am.ind
        am.pop.5[person,,] <- am.ind
        am.pop.6[person,,] <- am.ind
        am.pop.7[person,,] <- am.ind
        am.pop.8[person,,] <- am.ind
        bm.pop.1[person,,] <- bm.ind
        bm.pop.2[person,,] <- bm.ind
        bm.pop.3[person,,] <- bm.ind
        bm.pop.4[person,,] <- bm.ind
        bm.pop.5[person,,] <- bm.ind
        bm.pop.6[person,,] <- bm.ind
        bm.pop.7[person,,] <- bm.ind
        bm.pop.8[person,,] <- bm.ind
      }
      #mutation per site
      for (site in 1:dim(am.pop.1)[2]){
        am.pop.1[site+1,site,1] <- pmax(0.1,rnorm(1,mean=as.numeric(am.pop.1[site+1,site,1]),sd= 0.05)) #mutations in the first strand
        am.pop.2[site+1,site,2] <- pmax(0.1,rnorm(1,mean=as.numeric(am.pop.2[site+1,site,2]),sd= 0.05)) #mutations in the second strand
        am.pop.3[site+1,site,1] <- pmax(0.1,rnorm(1,mean=as.numeric(am.pop.3[site+1,site,1]),sd= 0.05)) #repeated as above
        am.pop.4[site+1,site,2] <- pmax(0.1,rnorm(1,mean=as.numeric(am.pop.4[site+1,site,2]),sd= 0.05)) 
        am.pop.5[site+1,site,1] <- pmax(0.1,rnorm(1,mean=as.numeric(am.pop.5[site+1,site,1]),sd= 0.05)) 
        am.pop.6[site+1,site,2] <- pmax(0.1,rnorm(1,mean=as.numeric(am.pop.6[site+1,site,2]),sd= 0.05)) 
        am.pop.7[site+1,site,1] <- pmax(0.1,rnorm(1,mean=as.numeric(am.pop.7[site+1,site,1]),sd= 0.05)) 
        am.pop.8[site+1,site,2] <- pmax(0.1,rnorm(1,mean=as.numeric(am.pop.8[site+1,site,2]),sd= 0.05)) 
        
        bm.pop.1[site+1,site,1] <- pmax(0.1,rnorm(1,mean=as.numeric(bm.pop.1[site+1,site,1]),sd= 0.05)) #mutations in the first strand
        bm.pop.2[site+1,site,2] <- pmax(0.1,rnorm(1,mean=as.numeric(bm.pop.2[site+1,site,2]),sd= 0.05)) #mutations in the second strand
        bm.pop.3[site+1,site,1] <- pmax(0.1,rnorm(1,mean=as.numeric(bm.pop.3[site+1,site,1]),sd= 0.05)) #repeated as above
        bm.pop.4[site+1,site,2] <- pmax(0.1,rnorm(1,mean=as.numeric(bm.pop.4[site+1,site,2]),sd= 0.05))
        bm.pop.5[site+1,site,1] <- pmax(0.1,rnorm(1,mean=as.numeric(bm.pop.5[site+1,site,1]),sd= 0.05))
        bm.pop.6[site+1,site,2] <- pmax(0.1,rnorm(1,mean=as.numeric(bm.pop.6[site+1,site,2]),sd= 0.05))
        bm.pop.7[site+1,site,1] <- pmax(0.1,rnorm(1,mean=as.numeric(bm.pop.7[site+1,site,1]),sd= 0.05))
        bm.pop.8[site+1,site,2] <- pmax(0.1,rnorm(1,mean=as.numeric(bm.pop.8[site+1,site,2]),sd= 0.05))
      }
      #before migration- combining population
      bm.pop[c(1:13),,] <- bm.pop.1
      bm.pop.2 <- bm.pop.2[-1,,] #remove first individual without mutation as already in population
      bm.pop.3 <- bm.pop.3[-1,,] #remove first individual without mutation as already in population
      bm.pop.4 <- bm.pop.4[-1,,] #remove first individual without mutation as already in population
      bm.pop.5 <- bm.pop.5[-1,,] #remove first individual without mutation as already in population
      bm.pop.6 <- bm.pop.6[-1,,] #remove first individual without mutation as already in population
      bm.pop.7 <- bm.pop.7[-1,,] #remove first individual without mutation as already in population
      bm.pop.8 <- bm.pop.8[-1,,] #remove first individual without mutation as already in population
      bm.pop[c(14:25),,] <- bm.pop.2
      bm.pop[c(26:37),,] <- bm.pop.3
      bm.pop[c(38:49),,] <- bm.pop.4
      bm.pop[c(50:61),,] <- bm.pop.5
      bm.pop[c(62:73),,] <- bm.pop.6
      bm.pop[c(74:85),,] <- bm.pop.7
      bm.pop[c(86:97),,] <- bm.pop.8
      #after migration
      am.pop[c(1:13),,] <- am.pop.1
      am.pop.2 <- am.pop.2[-1,,]
      am.pop.3 <- am.pop.3[-1,,]
      am.pop.4 <- am.pop.4[-1,,]
      am.pop.5 <- am.pop.5[-1,,]
      am.pop.6 <- am.pop.6[-1,,]
      am.pop.7 <- am.pop.7[-1,,]
      am.pop.8 <- am.pop.8[-1,,]
      am.pop[c(14:25),,] <- am.pop.2
      am.pop[c(26:37),,] <- am.pop.3
      am.pop[c(38:49),,] <- am.pop.4
      am.pop[c(50:61),,] <- am.pop.5
      am.pop[c(62:73),,] <- am.pop.6
      am.pop[c(74:85),,] <- am.pop.7
      am.pop[c(86:97),,] <- am.pop.8
      #calculate trait values
      #Before Migration
      bmy_1 <- (mu_values(bm.pop[,1,1],bm.pop[,2,1]) * negative_R_j(bm.y2.val,bm.pop[,7,1],bm.pop[,8,1])) + (mu_values(bm.pop[,1,2],bm.pop[,2,2]) * negative_R_j(bm.y2.val,bm.pop[,7,2],bm.pop[,8,2]))
      bmy_2 <- (mu_values(bm.pop[,5,1],bm.pop[,5,1]) * positive_R_j(bm.y1.val,bm.pop[,3,1],bm.pop[,4,1])) + (mu_values(bm.pop[,5,2],bm.pop[,5,2]) * positive_R_j(bm.y1.val,bm.pop[,3,2],bm.pop[,4,2]))
      bmy_3 <- (mu_values(bm.pop[,9,1],bm.pop[,9,1]) * positive_R_j(bmy_1,bm.pop[,11,1],bm.pop[,12,1])) + (mu_values(bm.pop[,9,2],bm.pop[,9,2]) * positive_R_j(bmy_1,bm.pop[,11,2],bm.pop[,12,2]))
      #After Migration
      amy_1 <- (mu_values(am.pop[,1,1],am.pop[,2,1]) * negative_R_j(am.y2.val,am.pop[,7,1],am.pop[,8,1])) + (mu_values(am.pop[,1,2],am.pop[,2,2]) * negative_R_j(am.y2.val,am.pop[,7,2],am.pop[,8,2]))
      amy_2 <- (mu_values(am.pop[,5,1],am.pop[,5,1]) * positive_R_j(am.y1.val,am.pop[,3,1],am.pop[,4,1])) + (mu_values(am.pop[,5,2],am.pop[,5,2]) * positive_R_j(am.y1.val,am.pop[,3,2],am.pop[,4,2]))
      amy_3 <- (mu_values(am.pop[,9,1],am.pop[,9,1]) * positive_R_j(amy_1,am.pop[,11,1],am.pop[,12,1])) + (mu_values(am.pop[,9,2],am.pop[,9,2]) * positive_R_j(amy_1,am.pop[,11,2],am.pop[,12,2]))
      #Get fitness from trait
      bm.fitness <- fitness(bmy_3,50,8)
      am.fitness <- fitness(amy_3,50,8)
      #ratio
      ratio.matrix[p,1] <- var(am.fitness) / var(bm.fitness)
    }
    ratio.list[[length(ratio.list)+1]] <- ratio.matrix
  }
  if (m == 1){
    all.ratio.matrix[1:220,1] <- as.numeric(unlist(ratio.list)) #saves matrices of the 44 conditions of the 4 environment/genetic conditions
  }else if (m == 2){
    all.ratio.matrix[221:440,1] <- as.numeric(unlist(ratio.list)) 
  }else if (m == 3){
    all.ratio.matrix[441:660,1] <- as.numeric(unlist(ratio.list)) 
  }else if (m == 4){
    all.ratio.matrix[661:880,1] <- as.numeric(unlist(ratio.list)) 
  }
}

#combination of anova and regression
#dataframe to save all those above  
main.pop.title <- rep(c('Heterozygous','Homozygous'),each=440)
migrant.pop.title <- rep(c('Heterozygous',"Homozygous",'Heterozygous',"Homozygous"),each=220)
migration.rate <- rep(c(0,0,0,0,0,-1,-1,-1,-1,-1,1,1,1,1,1,3,3,3,3,3,5,5,5,5,5,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,5,5,5,5,5,3,3,3,3,3,5,5,5,5,5),times=16)
migration.pattern <- rep(c(0,0,0,0,0,3,3,3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,2,2,2,2,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1),times=16)
result <- data.frame(main.pop.title,migrant.pop.title,migration.rate,migration.pattern,all.ratio.matrix,maxspeed.recovery.matrix)

#fitness variation during migration periods
wmigration.fitness.list <- list()
womigration.fitness.list <- list()
for (v in 1:4){
  list <- folder.list[[v]]
  migration.rda <- list[-c(1,12,23,34)]
  for (z in 1:length(migration.rda)){
    for (x in 1:5){
      load(paste0(migration.rda[z],'/run_1/Fitness/Simulation',x,'.rda'))
      wmigration.fitness.list[[length(wmigration.fitness.list)+1]] <- fit[c(80:700),1]
    }
  }
}
wmigration.fitness <- unlist(wmigration.fitness.list)
for (a in 1:4){
  list <- folder.list[[a]]
  womigration.rda <- list[c(1,12,23,34)]
  for (q in 1:length(womigration.rda)){
    for (w in 1:5){
      load(paste0(womigration.rda[q],'/run_1/Fitness/Simulation',w,'.rda'))
      womigration.fitness.list[[length(womigration.fitness.list)+1]] <- fit[c(80:700),1]
    }
  }
}
womigration.fitness <- unlist(womigration.fitness.list)

recovery.matrix <- data.frame(matrix(NA,nrow=880,ncol = 2))
#recovery times
recovery.result <- data.frame(result$migration.rate,result$migration.pattern,result$avg)
#diffgn environ. distance of 15 
mean(recovery.result$result.avg[1:220])
sd(recovery.result$result.avg[1:220])
#diffgn environ. distance of 30
mean(recovery.result$result.avg[221:440])
sd(recovery.result$result.avg[221:440])
#samegn environ. distance of 15 
mean(recovery.result$result.avg[441:660])
sd(recovery.result$result.avg[441:660])
#samegn environ. distance of 30
mean(recovery.result$result.avg[661:880])
sd(recovery.result$result.avg[661:880])
#boxplot
recovery.matrix[1:220,1] <- 1
recovery.matrix[1:220,2] <-recovery.result$result.avg[1:220]
recovery.matrix[221:440,1] <- 2
recovery.matrix[221:440,2] <-recovery.result$result.avg[221:440]
recovery.matrix[441:660,1] <- 3
recovery.matrix[441:660,2] <- recovery.result$result.avg[441:660]
recovery.matrix[661:880,1] <- 4
recovery.matrix[661:880,2] <- recovery.result$result.avg[661:880]
boxplot(recovery.matrix[,2]~recovery.matrix[,1],ylab = 'Recovery Time',xlab = 'Condition',main='Boxplot showing Recovery Time per Condition',cex.main=1.0)
legend('topright',legend = c('1 = Diff. w/ E.D 15','2 = Diff. w/ E.D 30','3 = Same w/ E.D 15','4 = Same w/ E.D 30'),cex=0.8)
#ANOVA of recovery times
#ANOVA of robustness
#remove random migration
no.random.result <- result[-which(result$migration.rate==-1 & result$migration.pattern==3),]
no.random.result$main.pop.title <- as.factor(no.random.result$main.pop.title)
no.random.result$migrant.pop.title <- as.factor(no.random.result$migrant.pop.title)
no.random.result$migration.rate <- as.factor(no.random.result$migration.rate)
no.random.result$migration.pattern <- as.factor(no.random.result$migration.pattern)
regression.test <- lm(log(Ratio)~migration.rate*migration.pattern+main.pop.title*migrant.pop.title,data=no.random.result)
anova.result <- anova(regression.test)
mp.regression <- lm(log(Ratio)~migration.pattern,data=no.random.result)
pdf("../Results/robustness_anova.pdf",height=7.5,width = 12)
grid.table(anova.result)
dev.off()
#preliminary checks
plot(mp.regression) #check qq plot
BARTLETT <- bartlett.test(log(Ratio)~migration.pattern, data = no.random.result) #check for homogeniety of 
pdf("../Results/bartlett_anova.pdf",height=7.5,width = 12)
grid.table(as.data.frame(unlist(BARTLETT)))
dev.off()

plot(log(Ratio)~migration.pattern,data=no.random.result,ylab = 'Robustness Ratio',xlab='Migration Pattern')
mtext("Regression Analysis of Robustness Ratio and Migration Pattern",side = 3, line = -2, outer = TRUE)
abline(lm(log(Ratio)~migration.pattern,data=no.random.result),col='red')
legend("bottom",xpd=TRUE,horiz=TRUE,inset=c(-0.2,-0.2), bty="n",legend = c("0 = Every gen.","1 = Every 10 gen","2 = Every 5 gen"), cex=0.8)

TUKEY <- TukeyHSD(aov(mp.regression)) #HSD test to see which interactions within pattern is significant
pdf("../Results/tukey_anova.pdf",height=7.5,width = 12)
grid.table(as.data.frame(unlist(TUKEY)))
dev.off()









