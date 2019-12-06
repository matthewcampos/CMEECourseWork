##plots two dataframes using Girko's circular law

##__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
##__version__ = '0.0.1'

Girko <- function(hradius, vradius, N){
  #takes in parameters of other function and size to return an ellipse
  require(ggplot2)
  build_ellipse <- function(hradius, vradius){ 
    # function that returns an ellipse
    npoints = 250
    a <- seq(0, 2 * pi, length = npoints + 1)
    v <- hradius * cos(a)
    c <- vradius * sin(a)  
    return(data.frame(v = v, c = c))
  }
  
  M <- matrix(rnorm(N * N), N, N) # Assign size and build the matrix
  
  eigvals <- eigen(M)$values # Find the eigenvalues
  
  eigDF <- data.frame("Real" = Re(eigvals), "Imaginary" = Im(eigvals)) # Build a dataframe
  
  my_radius <- sqrt(N) # The radius of the circle is sqrt(N)
  
  ellDF <- build_ellipse(my_radius, my_radius) # Dataframe to plot the ellipse
  
  names(ellDF) <- c("Real", "Imaginary") # rename the columns
  
  # plot the eigenvalues
  p <- ggplot(eigDF, aes(x = Real, y = Imaginary))
  p <- p +
    geom_point(shape = I(3)) +
    theme(legend.position = "none")
  
  # now add the vertical and horizontal line
  p <- p + geom_hline(aes(yintercept = 0))
  p <- p + geom_vline(aes(xintercept = 0))
  
  # finally, add the ellipse
  p <- p + geom_polygon(data = ellDF, aes(x = Real, y = Imaginary, alpha = 1/20, fill = "red"))
  
  # save in pdf
  pdf("../Results/Girko.pdf")
  print(p)
  dev.off()
  
  print(p)
}
