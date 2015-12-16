LabelsToBinary <- function(labels, label){
  # Create labels wicth contain 1 where it equal to label and 0 elsewhere
  #
  # Args:
  # labels: Training labels from 0 to 9
  # label: label for this class
  #
  # Returns:
  # Vector wicth contain 1 and 0
  
  y = labels
  
  # use value out of range to mark data
  y[y == label] = 10;
  
  # make not marked data equlas to 0
  y[y != 10] = 0;
  
  # set mark data to 1
  y[y == 10] = 1;
  
  return (y)
}

classifier <- function(data, labels, label){
  # Get classifier for label
  # 
  #
  # Args:
  # data: Training data
  # labels: Training labels
  # label: label for this class
  #
  # Returns:
  # Coefficient of sigmoid function
  
  # make labels for this class
  y = LabelsToBinary(labels, label)
  
  x <- data;
  
  # normalize data
  x <- x / 256.0 
  
  # Create coloumn with 1 for Theta1
  x <- cbind(1, x) 
  
  # Get numbers of features 
  nFeatures = dim(x)[2]
  
  # Set random values for Theta
  Theta = matrix(runif(nFeatures, -50, 50), nFeatures, 1)
  
  # initialize coefficients
  mu <- 10
  term <- 0.001
  prevJ <- term
  m <- length(y)
  
  repeat{
    
    # compute hypothesis here, for optimazing
    h <- 1 / (1.0 + exp(-x %*% Theta ))
    
    # calculate cost function
    J <- sum( -y*log(h) - (1 - y)*log(1 - h)) / m
    
    #if value is NaN or is.infinite that mean that sigmoid was round to 0 or 1. So we need some small extra value
    if (is.nan(J) || is.infinite(J))
    {
      #get smallest positive value
      smallest = .Machine$double.xmin
      #recalculate 
      J <- sum( -y*log(h + smallest) - (1 - y)*log(1 - h + smallest) ) / m
    }

    print( paste( "J = " , toString(J)))
    
    if (abs(J - prevJ) < term) {
      break;
    }
    
    # if we go to wrong direction that mean that we have to big "mu"
    if (J > prevJ){
      mu <- mu/2
      print( paste("New mu = " , toString(mu)))
    }
    
    # Save old J
    prevJ = J
    
    # compute new Theta
    Theta = Theta - mu *(t(x) %*% (h - y)/ m)    
  }
  
  print(J)
  
  return (Theta)
}