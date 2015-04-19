library(dplyr)

trainData <- read.csv("pml-training.csv", header = TRUE, stringsAsFactors = FALSE)
testData <-  read.csv("pml-testing.csv", header = TRUE, stringsAsFactors = FALSE)
#trainData <- mutate(trainData, classe = as.factor(classe), new_window = as.factor(new_window))
#M <- abs(cor(train))

badString <- function(str) {
  if (str == "#DIV/0!") {
    return (NA)
  } else {
    return(NA)
  }
}
# str == "#DIV/0!" ||
massageString <- function(str) {
  if ( str == "#DIV/0!" || str == "") { 
    return(badString(str))
  } else {
    return (as.numeric(str))
  } 
}

transformStrings <- function(xs) {
  if (is.character(xs)) {
    return(sapply(xs, massageString))
  }
  return (xs)
}

#numCols = ncol(trainData)
#trainData1 = trainData[,1:6]
#trainData2 = data.frame(sapply(7:numCols, function(i) { transformStrings(trainData[,i])}))
#names = names(trainData)
#names(trainData2) <- names[7:numCols]
#trainingData = cbind(trainData1,trainData2)

#nsv <- nearZeroVar(trainingData, saveMetrics = TRUE)
#train <- trainingData[,t(nsv[4])]

 
cleanData <- function(data) {
  data <- mutate(data, classe = as.factor(classe), new_window = as.factor(new_window))
  numCols = ncol(data)
  data1 = data[,1:6]
  data2 = data.frame(sapply(7:numCols, function(i) { transformStrings(data[,i])}))
  names = names(data)
  names(data2) <- names[7:numCols]
  res = cbind(data1,data2)  
  nsv <- nearZeroVar(res, saveMetrics = TRUE)
  res[,t(nsv[4])]  
}




