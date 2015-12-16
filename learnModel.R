learnModel <- function(data, labels){
  # Learn model. Model may be lear from file or by computition. 
  # If you need compute data, just delete files from 
  # SavedClassifier directory 
  #
  # Args:
  # data: Training data
  # labels: labels for training data
  #
  # Returns:
  # Matrix of classifaer cofficient. n coloumn if coefficint Theta for n-1 label
  
  learnModel = 0
  fileName = "SavedClassifiers\\data 0.csv"
  
  x <- cbind(1, data/256.0)
  
  if (file.exists(fileName)){
    # load data from file
    learnModel <- read.table(fileName, header=T, sep=",")
    
    # make data numeric
    learnModel <- as.matrix(learnModel)[,2]
    
    # create labels for this class
    y = LabelsToBinary(labels, 0)
    
    print(sprintf("Classifier 0 was load from file"))
    
    h <- 1 / (1.0 + exp(-x %*% learnModel ))
    m <- length(y)
    J <- sum( -y*log(h) - (1 - y)*log(1 - h)) / m
    if (is.nan(J) || is.infinite(J))
    {
      #get smallest positive value
      smallest = .Machine$double.xmin
      #recalculate 
      J <- sum( -y*log(h + smallest) - (1 - y)*log(1 - h + smallest) ) / m
    }
    print(sprintf("J for 0-class = %g", J))
  }
  else {
    # compute coefficient
    learnModel <- classifier(data, labels, 0)
    
    # save data
    write.csv(learnModel, file = fileName)
    print("Classifier 0 compute")
  }

    for (i in 1:9) {
    
    fileName = paste("SavedClassifiers\\data", toString(i))
    fileName = paste(fileName, ".csv")
    
    if (file.exists(fileName)){
      # load data from file
      tmp <- read.table(fileName, header=T, sep=",")
      # make data numeric
      
      tmp <- as.matrix(tmp)[,2]
      print(sprintf("Classifier %g was load from file", i))
    }
    else {
      # compute coefficient
      tmp <- classifier(data, labels, i)
      
      # save data
      write.csv(tmp, file = fileName)
      print(paste(toString(i), " classifier compute"))
    }
    
    # Make binary labels for i class
    y = LabelsToBinary(labels, i)
    
    # compute cost function for i class
    h <- 1 / (1.0 + exp(-x %*% tmp))
    m <- length(y)
    J <- sum( -y*log(h) - (1 - y)*log(1 - h)) / m
    
    if (is.nan(J) || is.infinite(J))
    {
      #get smallest positive value
      smallest = .Machine$double.xmin
      #recalculate 
      J <- sum( -y*log(h + smallest) - (1 - y)*log(1 - h + smallest) ) / m
    }
    
    print(sprintf("J for %g-class = %g",i,  J))

    # bind matrix with vector
    learnModel <- cbind(learnModel, tmp)
  }
  return (learnModel)
}