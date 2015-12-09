Sigmoid <- function(Theta, X){
  # Calculate sigmoid function
  # 
  #
  # Args:
  # Theta: coefficient of sigmoid. Dim: m x 1
  # X: features of sigmoid. First column must be 1. Dim n x m.
  
  # Returns:
  # Vector with values of sigmoid.
  Sigmoid <- 1 / (1.0 + exp(-X %*% Theta ))
}

CostFunction <- function(Theta, x, y){
  # Calculate cost function
  # 
  #
  # Args:
  # Theta: coefficient of model. Dim: m x 1
  # X: features of model. First column must be 1. Dim n x m.
  # Y: Existans values for model.
  
  m = length(y)
  CostFunction <- sum( -y*log(Sigmoid(Theta, x)) - (1 - y)*log(1 - Sigmoid( Theta, x)) ) / m
  #if value is NaN or is.infinite that mean that sigmoid was round to 0 or 1. So we need some small extra value
  if (is.nan(CostFunction) || is.infinite(CostFunction))
  {
    #get smallest positive value
    smallest = .Machine$double.xmin
    #recalculate 
    CostFunction <- sum( -y*log(Sigmoid(Theta, x) + smallest) - (1 - y)*log(1 - Sigmoid( Theta, x)+ smallest) ) / m
  }
    
  return (CostFunction)
}

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
  
  nTheta <- Theta
  
  a <- 10
  term <- 0.001
  prevJ <- term
  m <- length(y)
  
  J = CostFunction(Theta, x, y)
  
  while(abs(J - prevJ) > term){
    # compute hypotise here, for optimazing
    h <- Sigmoid(Theta, x) 
    for (i in 1:(nFeatures)) {
      nTheta[i] <- sum((h - y)*x[, i]) / m 
    }
    
    Theta = Theta - a*nTheta
    prevJ = J
    J = CostFunction(Theta, x, y)
    print( paste( "J = " , toString(J)))
    
    # if we go to wrong direction that mean that we have to big "a"
    if (J > prevJ){
      a <- a/2
      print( paste("New a = " , toString(a)))
    }
  }
  print(J)
  
  return (Theta)
}