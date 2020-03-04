##Equations for model fitting

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

#functions for growth
Logistic_model <- function(t, r_max, N_max, N_0){ # The classic logistic equation
  return(log10(N_0 * N_max * exp(r_max * t)/(N_max + N_0 * (exp(r_max * t) - 1)))) #take common logarithm
}
#classic logistic
#as r increases,the less time it takes for the population to reach k so goes from s shape to r shape
#as k increases, the more time it takes for the population to reach k so more time before it exponentially grows rapidly

Baranyi_model <- function(t, r_max, N_max, N_0, t_lag){  # Baranyi model (Baranyi 1993)
  return(N_max + log10((-1+exp(r_max*t_lag) + exp(r_max*t))/(exp(r_max*t) - 1 + exp(r_max*t_lag) * 10^(N_max-N_0))))
}
#increasing h0 causes lag before increase and can cause decrease if greater than 1
#increasing k causes graph to become linear
#increasing r causes it to become r shape faster

Gompertz_model <- function(t, r_max, N_max, N_0, t_lag){ # Modified gompertz growth model (Zwietering 1990)
  return(N_0 + (N_max - N_0) * exp(-exp(r_max * exp(1) * (t_lag - t)/((N_max - N_0) * log(10)) + 1)))
}
#increasing tlag causes the exponential increase to occur later
#increasing r causes the exponential increase to occur more rapidly
#increasing k causes the exponential increase line to become more linear

Buchanan_model <- function(t, r_max, N_max, N_0, t_lag){ # Buchanan model - three phase logistic (Buchanan 1997)
  return(N_0 + (t >= t_lag) * (t <= (t_lag + (N_max - N_0) * log(10)/r_max)) * r_max * (t - t_lag)/log(10) + (t >= t_lag) * (t > (t_lag + (N_max - N_0) * log(10)/r_max)) * (N_max - N_0))
}
#decreasing k lessens the time taken to reach carrying capacity
#increasing tlag causes the time for linear increase to occur later
#increasing r makes the linear growth steeper
#smaller starting value takes longer to reach carrying capacity














