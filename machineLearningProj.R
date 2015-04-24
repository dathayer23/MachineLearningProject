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
hasNoNulls <- function(data) {
  nas = is.na(data)
  sum(nas) == 0
}

validCol <- function(data) {
  (is.numeric(data) || is.ordered(data) || is.factor(data)) && hasNoNulls(data)
}

validCols <- function(train) { 
  sapply(1:ncol(train) , function(i) { validCol(train[,i])} )
}

cleanCol <- function(data) {
   data <- as.numeric(data)  
   data
} 

cleanCols <- function(train) {
  res <- mutate(train, 
        X = cleanCol(X),
        raw_timestamp_part_1 = cleanCol(raw_timestamp_part_1),
        raw_timestamp_part_2 = cleanCol(raw_timestamp_part_2),
        num_window = cleanCol(num_window),
        total_accel_belt = cleanCol(total_accel_belt),
        accel_belt_x = cleanCol(accel_belt_x),
        accel_belt_y = cleanCol(accel_belt_y),
        accel_belt_z = cleanCol(accel_belt_z),
        magnet_belt_x = cleanCol(magnet_belt_x),
        magnet_belt_y = cleanCol(magnet_belt_y),
        magnet_belt_z = cleanCol(magnet_belt_z),
        accel_arm_x = cleanCol(accel_arm_x),
        accel_arm_y = cleanCol(accel_arm_y),
        accel_arm_z = cleanCol(accel_arm_z),
        magnet_arm_x = cleanCol(magnet_arm_x),
        magnet_arm_y = cleanCol(magnet_arm_y),
        magnet_arm_z = cleanCol(magnet_arm_z),
        total_accel_dumbbell = cleanCol(total_accel_dumbbell),
        accel_dumbbell_x = cleanCol(accel_dumbbell_x),
        accel_dumbbell_y = cleanCol(accel_dumbbell_y),
        accel_dumbbell_z = cleanCol(accel_dumbbell_z),
        magnet_dumbbell_x = cleanCol(magnet_dumbbell_x),
        magnet_dumbbell_y = cleanCol(magnet_dumbbell_y),
        total_accel_forearm = cleanCol(total_accel_forearm),
        accel_forearm_x = cleanCol(accel_forearm_x),
        accel_forearm_y = cleanCol(accel_forearm_y),
        accel_forearm_z = cleanCol(accel_forearm_z))
  res
}

cleanData <- function(data) {
  #data <- mutate(data, classe = as.factor(classe), new_window = as.factor(new_window))
  valid <- validCols(data)
  #numCols = ncol(data)
  #data1 = data[,1:6]
  #data2 = data.frame(sapply(7:numCols, function(i) { transformStrings(data[,i])}))
  #names = names(data)
  #names(data2) <- names[7:numCols]
  data <- data[,valid] # ind(data1,data2)    
  #dims <- dim(data)
  names <- names(data)
  data <- cleanCols(data)
  names(data) <- names
  #dim(data) <- dims
  #names(data) <- names
  data
}

model <- NULL
accuracy <- NULL
pred <- NULL

trainAndPredict <- function(train, test, methodUsed) {
  class = as.numeric(factor(train$classe, labels = c("1","2","3","4","5"))) 
  testclass = as.numeric(factor(test$classe, labels = c("1","2","3","4","5")))
  train = cleanData(train)
  test = cleanData(test)
  model <<- train(x = train, y = class, preProcess="pca", method = methodUsed)
  pred <<- round(predict(model, test))
  mismatches = (testclass - pred) != 0
  accuracy <<- 1.0 - sum(mismatches)/length(testclass)
}

set.seed(31415)
training = createDataPartition(trainSet$classe, p = 0.5, list=FALSE)
train = trainSet[training,]
test = trainSet[-training,]

trainAndPredict(train,test, "glm")







