##getting parameter values and fitting models

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

library(minpack.lm) 
library(dplyr)
library(gridExtra)

rda_list <- list.files(path = "../Data/RDA",full.names = TRUE) #load RDA files

#check starting parameter values
starting_parameter_values <- matrix(NA, nrow = length(rda_list), ncol = 4)
colnames(starting_parameter_values) <- c("r_max", "N_0", "N_max", "t_lag")
for (i in 1:length(rda_list)){
  tryCatch({
  load(rda_list[i])
  starting_parameter_values[i,1] <- r_value(data)[1]
  starting_parameter_values[i,2] <- N_max_N0_parameter_values(data)[1]
  starting_parameter_values[i,3] <- N_max_N0_parameter_values(data)[2]
  starting_parameter_values[i,4] <- t_lag_parameter_value(data)
  },error=function(e){}) #trycatch to ignore errors 
}
table(is.na(starting_parameter_values[,1])) #counts how many NA in r_max and t_lag
dev.off()

#create dataframe with AIC values
aic_results_df <- data.frame(matrix(ncol = 4, nrow = length(rda_list)))
names_col <- c("Logistic", "Baranyi", "Gompertz", "Buchanan")
colnames(aic_results_df) <- names_col

pdf(paste("../Results/NLSFit.pdf")) #open pdf
#make the outer margin at the bottom of the plot large
par(oma = c(1, 0.5, 0.5, 0.5))
for (len in 1:length(rda_list)){
  tryCatch({
  load(rda_list[len])
  r_max_start <- r_value(data)[1]
  N_0_start <- N_max_N0_parameter_values(data)[1]
  N_max_start <- N_max_N0_parameter_values(data)[2]
  t_lag_start <- t_lag_parameter_value(data)
  x <- seq(min(data),max(data))
  
  #Plot data
  plot(data,pch=19,xlab="Time",y="Log10(Abundance)",main=c(ID[1],ID[2],ID[3],ID[4]),cex=0.8)
  legend("bottom",xpd=TRUE,horiz=TRUE,inset=c(-0.2,-0.2), bty="n",legend = c("Logistic","Baranyi","Gompertz","Buchanan"),col=c("blue", "red","purple","green"), lty=1:4, cex=0.8)
  
  #Logistic Fit
  fit_logistic <- nlsLM(data$Abundance ~ Logistic_model(t = data$Time, r_max, N_max, N_0), data,
             list(r_max = r_max_start, N_0 = N_0_start, N_max = N_max_start))
  Logistic_AIC <- AIC(fit_logistic)
  summary_logistic <- summary(fit_logistic)
  r_fit <- summary_logistic$coefficients[1,1]
  N0_fit <- summary_logistic$coefficients[2,1]
  K_fit <- summary_logistic$coefficients[3,1]
  logistic_points <- Logistic_model(t=x, r_max = r_fit, N_max = K_fit, N_0 = N0_fit)
  lines(logistic_points~x, col="blue",lty=1,lwd=2)
  aic_results_df[len,1] <- Logistic_AIC
  
  #Baranyi Fit
  fit_baranyi <- nlsLM(data$Abundance ~ Baranyi_model(t = data$Time, r_max, N_max, N_0, t_lag), data,
                       list(t_lag=t_lag_start, r_max=r_max_start, N_0 = N_0_start, N_max = N_max_start))
  Baranyi_AIC <- AIC(fit_baranyi)
  summary_baranyi <- summary(fit_baranyi)
  t_lag_fit <- summary_baranyi$coefficients[1,1]
  r_fit <- summary_baranyi$coefficients[2,1]
  N0_fit <- summary_baranyi$coefficients[3,1]
  K_fit <- summary_baranyi$coefficients[4,1]
  baranyi_points <- Baranyi_model(t=x, r_max = r_fit, N_max = K_fit, N_0 = N0_fit, t_lag = t_lag_fit)
  lines(baranyi_points~x, col="red",lty=2,lwd=2)
  aic_results_df[len,2] <- Baranyi_AIC

  #Gompertz fit
  fit_gompertz <- nlsLM(data$Abundance ~ Gompertz_model(t = data$Time, r_max, N_max, N_0, t_lag), data,
                        list(t_lag=t_lag_start, r_max=r_max_start, N_0 = N_0_start, N_max = N_max_start))
  Gompertz_AIC <- AIC(fit_gompertz)
  summary_gompertz <- summary(fit_gompertz)
  t_lag_fit <- summary_gompertz$coefficients[1,1]
  r_fit <- summary_gompertz$coefficients[2,1]
  N0_fit <- summary_gompertz$coefficients[3,1]
  K_fit <- summary_gompertz$coefficients[4,1]
  gompertz_points <- Gompertz_model(t=x, r_max = r_fit, N_max = K_fit, N_0 = N0_fit, t_lag = t_lag_fit)
  lines(gompertz_points~x, col="purple",lty=3,lwd=2)
  aic_results_df[len,3] <- Gompertz_AIC

  #Buchanan Fit
  fit_buchanan <- nlsLM(data$Abundance ~ Buchanan_model(t = data$Time, r_max, N_max, N_0, t_lag), data,
                        list(t_lag=t_lag_start, r_max=r_max_start, N_0 = N_0_start, N_max = N_max_start))
  Buchanan_AIC <- AIC(fit_buchanan)
  summary_buchanan <- summary(fit_buchanan)
  t_lag_fit <- summary_buchanan$coefficients[1,1]
  r_fit <- summary_buchanan$coefficients[2,1]
  N0_fit <- summary_buchanan$coefficients[3,1]
  K_fit <- summary_buchanan$coefficients[4,1]
  buchanan_points <- Buchanan_model(t=x, r_max = r_fit, N_max = K_fit, N_0 = N0_fit, t_lag = t_lag_fit)
  lines(buchanan_points~x, col="green",lty=4,lwd=2)
  aic_results_df[len,4] <- Buchanan_AIC

  }, error=function(e){}) #trycatch to ignore errors 
}
dev.off()

#count the number of fits each model had
Logistic_fit <- length(na.omit(aic_results_df[,1]))
Baranyi_fit <- length(na.omit(aic_results_df[,2]))
Gompertz_fit <- length(na.omit(aic_results_df[,3]))
Buchanan_fit <- length(na.omit(aic_results_df[,4]))

#count best model fit
best_fit <- matrix(0,nrow = 1, ncol = 4)
colnames(best_fit) <- names_col
for (i in 1:length(rda_list)){
  check <- rowSums(aic_results_df[i,],na.rm = TRUE) #checks for NA as sum would be 0
  if (check == 0){
    next
  }
  val_max <- which(aic_results_df[i,]==min(aic_results_df[i,],na.rm = TRUE)) #value is column with highest AIC
  if (val_max == 1){
    best_fit[1,1] <- best_fit[1,1] + 1
  }else if (val_max == 2){
    best_fit[1,2] <- best_fit[1,2] + 1
  }else if (val_max == 3){
    best_fit[1,3] <- best_fit[1,3] + 1
  }else{
    best_fit[1,4] <- best_fit[1,4] + 1
  }
}

#barplot to visualise best fit
bp <- barplot(best_fit,main="Lowest AIC Count",ylab="count",col = "grey")
text(bp, 0, as.numeric(best_fit), cex=1, pos=3) #add values to each bar plot
dev.off()

#find NA and convert to 0
LNA_index <- which(is.na(aic_results_df$Logistic)==TRUE) #find NA index in Logistic column
aic_results_df$Logistic[LNA_index] <- 0 #set value to 0
BNA_index <- which(is.na(aic_results_df$Baranyi)==TRUE) #find NA index in Baranyi column
aic_results_df$Baranyi[BNA_index] <- 0
GNA_index <- which(is.na(aic_results_df$Gompertz)==TRUE) #find NA index in Gompertz column
aic_results_df$Gompertz[GNA_index] <- 0
BuNA_index <- which(is.na(aic_results_df$Buchanan)==TRUE) #find NA index in Buchanan column
aic_results_df$Buchanan[BuNA_index] <- 0
Inf_index <- which(aic_results_df$Buchanan==-Inf) #find Inf index in Buchanan column
aic_results_df$Buchanan[Inf_index] <- 0
result <- colSums(aic_results_df != 0) #get count for each column for values not equal to 0

#barplot
bp2 <- barplot(result,main="Fit Count",ylab="count",col = "grey")
text(bp2, 0, as.numeric(result), cex=1, pos=3) #add values to each bar plot
dev.off()







