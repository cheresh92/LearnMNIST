testModel <- function(classifier, Data){
  # Test model for checking.
  #
  # Args:
  # classifier: Coefficients of Sigmoid. Coloumn represent one class
  # Data: features of model
  #
  # Returns:
  # Label of data: from 0 to 9
  
  # normalize data
  x <- Data / 256.0 
  x <- cbind(1, x)
  
  # coupute value for 0-class
  res <- Sigmoid(matrix(classifier[, 1], dim(classifier)[1], 1), x)
  
  # coupute value for i-class
  for (i in 2:9) {
    res <- cbind(res, Sigmoid(classifier[, i], x))
  }
  
  finalLabel <- matrix(0, dim(x)[1], 1)
  for (i in 1:(dim(x)[1])) {
    
    # finding index of maxim value in a row
    # index number - 1 is label 
    finalLabel[i] <- which.max(res[i, ])-1
  }
  
  return (finalLabel)
}