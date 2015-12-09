classMetric <- function(Thetas, data, labels, label){
  # Print class metric. 
  # Thera are recall, precision, specificity, F-measure, FDR 
  # and ROC curve
  #
  # Args:
  # Thetas
  # data: Training data
  # labels: labels for training data
  # label
  
  y <- LabelsToBinary(labels, label)
  
  x <- data;
  # Normalize
  x <- x / 256.0  
  # Create coloumn with 1 for Theta1
  x <- cbind(1, x)  
  
  
  predictedValues <- Sigmoid(Thetas, x)
  predictedLabels <- as.numeric(predictedValues > 0.5) 
  
  # predictedLabels and labels binary matrix. If we multyply them
  # it will be like binary and. 
  # Then we just get number of "1" in result of multiplication
  tp <- as.numeric( table(predictedLabels * labels)["1"])
  tn <- as.numeric( table( as.numeric(!predictedLabels) * as.numeric(!labels) )["1"])
  fn <- as.numeric( table( as.numeric(predictedLabels) * as.numeric(!labels) )["1"])
  fp <- as.numeric( table( as.numeric(!predictedLabels) * as.numeric(labels) )["1"])
  
  # normalize metrics
  tp <- tp / length(labels)
  tn <- tn / length(labels)
  fn <- fn / length(labels)
  fp <- fp / length(labels)
  
  
  # compute data
  recall <- tp / (tp + fn)
  precision <- tp / (tp + fp)
  specificity <- tn / (tn + fp)
  Fmeasure <- 2*tp /(2*tp + fn + fp)
  FDR <- fp / (tp + fp)
  
  print( sprintf("For %g: recall = %g, precision = %g, specificity = %g, Fmeasure = %g,  FDR = %g",label, recall, precision, specificity, Fmeasure, FDR))
  
  # plot ROC curve
  pred <- prediction(predictedValues, y)
  roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
  plot(roc.perf, main = paste("class ", toString(label)))
}