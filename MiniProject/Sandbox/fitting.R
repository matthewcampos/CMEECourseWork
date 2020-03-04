##getting parameter values and fitting models

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

require(minpack.lm)

#saving parameters in a vector
rda_list <- list.files(path = "../Data/RDA",full.names = TRUE) #load RDA files
data$Abundance
k_vector <- c()
N0_vector <- c()
r_vector <- c()
t_lag_vector <- c()
count <-0
for (len in len){
  count <- count + 1
  print(count)
  load(rda_list[len])
  data <- data
  inf <- c()
  for (i in 1:length(data$Abundance)){
    if (data$Abundance[i] == -Inf){
      inf <- c(i,inf)
    }
  }
  if (length(inf)>0){
    data <- data[-inf, ]
  }
  K <- K_N0_parameter_values(data)[2]
  N0 <- K_N0_parameter_values(data)[1]
  r <- r_parameter_values(data)[1]
  t_lag<-t_lag_parameter_value(data)
  k_vector <- c(k_vector,K)
  N0_vector <- c(N0_vector,N0)
  r_vector <- c(r_vector,r)
  t_lag_vector <- c(t_lag_vector,t_lag)
}

#plotting r slope on data
rda_list <- list.files(path = "../Results/RDA",full.names = TRUE) #load RDA files
pdf(paste("../Results/Species_ParameterFit_ID.pdf")) #open pdf
count <-0
for (len in 1:length(rda_list)){
  count <- count + 1
  print(count)
  load(rda_list[len])
  data <- data
  inf <- c()
  y <- c()
  for (i in 1:length(data$Abundance)){
    if (data$Abundance[i] == -Inf){
      inf <- c(i,inf)
    }
  }
  if (length(inf)>0){
    data <- data[-inf, ]
  }
  plot(data[,2]~data[,1])
  result_slope <- r_parameter_values(data)[1]
  reuslt_int <- r_parameter_values(data)[2]
  x <- data$Time
  y <- c(result_slope*x+result_slope,y)
  lines(y~x)
}
dev.off() #close pdf file with all graphs

#check if r values being produced 
rda_list <- list.files(path = "../Results/RDA",full.names = TRUE) #load RDA files
count <-0
par(mfrow=c(2,2))
for (len in len){
  count <- count + 1
  print(count)
  load(rda_list[len])
  data <- data
  inf <- c()
  y <- c()
  for (i in 1:length(data$Abundance)){
    if (data$Abundance[i] == -Inf){
      inf <- c(i,inf)
    }
  }
  if (length(inf)>0){
    data <- data[-inf, ]
  }
  r_parameter_values(data)
}

#Model fit
rda_list <- list.files(path = "../Results/RDA",full.names = TRUE) #load RDA files
pdf(paste("../Results/Species_Fit_ID.pdf")) #open pdf
count <- 0
for (len in 1:length(rda_list)){
  count <- count + 1
  print(count)
  load(rda_list[len])
  data <- data
  inf <- c()
  for (i in 1:length(data$Abundance)){
    if (data$Abundance[i] == -Inf){
      inf <- c(i,inf)
    }
  }
  if (length(inf)>0){
    data <- data[-inf, ]
  }
  plot(data[,2]~data[,1])
  K <- K_N0_parameter_values(data)[2]
  N0 <- K_N0_parameter_values(data)[1]
  r <- r_parameter_values(data)[1]
  t_lag<-t_lag_parameter_value(data)
  #nlsLM(data$Abundance ~ Modified_Logistic_model(t_lag, N0, K, r, t = data$Time), data = data, list(N0 = N0, k = K, r = r, t_lag = t_lag) )
  x<-lm(data[,2]~poly(data[,1],2))
  y<-lm(data[,2]~poly(data[,1],3))
  lines(data[,1],predict(x),col="red")
  lines(data[,1],predict(y),col="blue")
}
dev.off() #close pdf file with all graphs

pdf(paste("../Results/Species_NLM_Fit_ID.pdf")) #open pdf
rda_list <- list.files(path = "../Data/RDA",full.names = TRUE) #load RDA files
count <- 0
for (len in len){
  tryCatch({
    count <- count + 1
    print(count)
    load(rda_list[len])
    data <- data
    inf <- c()
    for (i in 1:length(data$Abundance)){
      if (data$Abundance[i] == -Inf){
        inf <- c(i,inf)
      }
    }
    if (length(inf)>0){
      data <- data[-inf, ]
    }
    plot(data[,2]~data[,1])
    K <- K_N0_parameter_values(data)[2]
    N0 <- K_N0_parameter_values(data)[1]
    r <- r_parameter_values(data)[1] 
    t_lag<-t_lag_parameter_value(data)
    x <-nlsLM(data$Abundance ~ Logistic_model(N0, K, r, t = data$Time), data = data, list(N0 = N0, k = K, r = r) )
    lines(data$Time, Logistic_model(N0, K, r, t = data$Time)) #to find r log data[,2] and then get slope of first few datapoints
    print(summary(x)) 
    #x<-lm(data[,2]~poly(data[,1],2))
    #y<-lm(data[,2]~poly(data[,1],3))
    #lines(data[,1],predict(x),col="red")
    #lines(data[,1],predict(y),col="blue")
  }, error = function(e){})
  }
dev.off() #close pdf file with all graphs

#create dataframe with AIC values
aic_results_df <- data.frame(matrix(ncol = 6, nrow = 285))
names_col <- c("Logistic", "Baryani", "Gompertz", "Buchanan", "Quadratic", "Cubic")
colnames(aic_results_df) <- names_col





