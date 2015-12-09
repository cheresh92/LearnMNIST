learnModel <- function(data, labels){
  # Learn model. Model may be lear from file or by computition. 
  # If you need compute data, just delete files from 
  # SavedClasifier directory 
  #
  # Args:
  # data: Training data
  # labels: labels for training data
  #
  # Returns:
  # Matrix of classifaer cofficient. n coloumn if coefficint Theta for n-1 label
  
  learnModel = 0
  fileName = "SavedClasifier\\data 0.csv"
  
  if (file.exists(fileName)){
    # load data from file
    learnModel <- read.table(fileName, header=T, sep=",")
    
    # make data numeric
    learnModel <- as.matrix(learnModel)[,2]
    
    # create labels for this class
    y = LabelsToBinary(labels, 0)
    
    print(sprintf("Classifier 0 was load from file"))
    print(sprintf("J for 0-class = %g", CostFunction(learnModel, cbind(1, data/256.0), y)))
  }
  else {
    # compute coefficient
    learnModel <- classifier(data, labels, 0)
    
    # save data
    write.csv(learnModel, file = fileName)
    print("Classifier 0 compute")
  }

    for (i in 1:9) {
    
    fileName = paste("SavedClasifier\\data", toString(i))
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
    print(sprintf("J for %g-class = %g",i,  CostFunction(tmp, cbind(1, data/256.0), y)))

    # bind matrix with vector
    learnModel <- cbind(learnModel, tmp)
  }
  return (learnModel)
}