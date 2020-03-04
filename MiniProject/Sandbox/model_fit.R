##getting parameter values and fitting models

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

require(minpack.lm)
rda_list <- list.files(path = "../Data/RDA",full.names = TRUE) #load RDA files

#check if r values being produced 
count <-0
par(mfrow=c(2,2))
for (len in 1:length(rda_list)){
  count <- count + 1
  print(count)
  load(rda_list[len])
  data <- data
  r_value(data)
}

#Model Fits
pdf(paste("../Results/Species_NLM_Fit_ID.pdf")) #open pdf
count <- 0
for (len in 1:length(rda_list)){
  count <- count + 1
  print(count)
  load(rda_list[len])
  plot(log(data[,2])~data[,1], main=len)
  K <- K_N0_parameter_values(data)[2]
  N0 <- K_N0_parameter_values(data)[1]
  r <- r_max_value(data)[1] 
  t_lag<-t_lag_parameter_value(data)
  nls <-nlsLM(data$Abundance ~ Logistic_model(N0, K, r, t = data$Time), data = data, list(N0 = N0, k = K, r = r) )
  nls_summary <- summary(nls_summary)
  lines(data$Time, log(Logistic_model(N0, K, r, t = data$Time)))
  
  #Phenomenological models
  quadratic<-lm(data[,2]~poly(data[,1],2))
  cubic<-lm(data[,2]~poly(data[,1],3))
  lines(data[,1],predict(quadratic),col="red")
  lines(data[,1],predict(cubic),col="blue")

}
dev.off() #close pdf file with all graphs

#create dataframe with AIC values
aic_results_df <- data.frame(matrix(ncol = 6, nrow = 285))
names_col <- c("Logistic", "Baryani", "Gompertz", "Buchanan", "Quadratic", "Cubic")
colnames(aic_results_df) <- names_col


#lines(data$Time, Baryani_model(t_lag,N0,K,r,t=data$Time), col="red")
#lines(data$Time, Gompertz_model(t_lag,N0,K,r,t=data$Time), col="blue")
#lines(data$Time, Buchanan_model(t_lag,N0,K,r,t=data$Time), col="green")

#  data <- data
#data[,2]<- log(data[,2])
#inf <- c()
#for (i in 1:length(data$Abundance)){
#  if (data$Abundance[i] == -Inf){
#    inf <- c(i,inf)
#  }
#}
#if (length(inf)>0){
#  data <- data[-inf, ]
#}