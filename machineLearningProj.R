library(dplyr)
library(caret)
library(kernlab)

print("Load Training and Test Data")
trainSet <- read.csv("pml-training.csv", header = TRUE, stringsAsFactors = FALSE)
testSet <-  read.csv("pml-testing.csv", header = TRUE, stringsAsFactors = FALSE)

#badString <- function(str) {
#  if (str == "#DIV/0!") {
#    return (NA)
#  } else {
#    return(NA)
#  }
#}
# str == "#DIV/0!" ||
#massageString <- function(str) {
#  if ( str == "#DIV/0!" || str == "") { 
#    return(badString(str))
#  } else {
#   return (as.numeric(str))
#  } 
#}

#transformStrings <- function(xs) {
#  if (is.character(xs)) {
#    return(sapply(xs, massageString))
#  }
#  return (xs)
#}

# does this data column have any NA values
hasNoNulls <- function(data) {
  nas = is.na(data)
  sum(nas) == 0
}

# column is either numeric, ordered, or factor and has no NA values
validCol <- function(data) {
  (is.numeric(data) || is.ordered(data) || is.factor(data)) && hasNoNulls(data)
}

# check that all columns in a data frame is a valid column
validCols <- function(train) { 
  sapply(1:ncol(train) , function(i) { validCol(train[,i])} )
}

# convert data column to numeric
cleanCol <- function(data) {
   data <- as.numeric(data)  
   data
} 

# convert all integer columns in a data frame to numeric
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

# select valid cols and convert them to numeric
cleanData <- function(data) {
  valid <- validCols(data)
  data <- data[,valid] # ind(data1,data2)    
  names <- names(data)
  data <- cleanCols(data)
  names(data) <- names
  data
}

# replace value in data that are equal to src with the value target
replaceValue <- function(data, src, target) {
  sapply(data, function(v) { ifelse(v == src, target, v )})
}

# transate all levels in pred collection to a zero if it is not in the src collection 
adjustLevels <- function(src, pred) {
  levels = as.numeric(levels(as.factor(src)))
  pred <- sapply(pred, function(v) { ifelse(v %in% levels, v, 0) })
}

# take the values predicted by the regression model and round it off to the nearest integer to get a 
# proposed class label.  Assume that a value under the lowest class value is potentiallythe lowest class value 
# and conversely that a value higher than the top class is actually the top class.  This makes all the 
# class ids a valid id whether or not it is correct
predictAndPostProcess <- function(model, data) {
  pred1 <<- round(predict(model, data))
  #predictions of 0 are assumed to be 1 and predictions greater than 5 are assumed to be 5
  pred <- replaceValue(pred1, 0, 1)
  pred <<- replaceValue(pred, 6, 5)
  pred
}

#output of train and validate function
accuracy <- NULL
pred <- NULL
pred1 <- NULL
testclass <- NULL

# take the testdata and train using a specific training regime and a training method and preprocesser 
# specifications.  The reason that this method uses regression is that it translates the classe column 
# into a numeric value and trains to predict that numeric value
trainAndValidateRegression <- function(train, test, methodUsed, trainControl, preProcesser) {
  trainclass = as.numeric(factor(train$classe, labels = c("1","2","3","4","5"))) 
  testclass <<- as.numeric(factor(test$classe, labels = c("1","2","3","4","5")))
  train = cleanData(train)
  test = cleanData(test)
  model <<- train(x = train, y = trainclass, preProcess = preProcesser, method = methodUsed, tuneLength = 3, trControl = trainControl)
  pred <<- predictAndPostProcess(model, test)
  
  mismatches = (testclass - pred) != 0
  accuracy <<- 1.0 - sum(mismatches)/length(testclass)
  model
}

# take the testdata and train using a specific training regime and a training method and preprocesser 
# specifications.  The reason that this method uses classification is that it translates the classe column 
# into a factor value and trains to predict that factor
trainAndValidateClassification <- function(train, test, methodUsed, trainControl, preProcesser) {
  trainclass = as.factor(train$classe) 
  testclass <<- as.factor(test$classe)
  train = cleanData(train)
  test = cleanData(test)
  model <<- train(x = train, y = trainclass, preProcess = preProcesser, method = methodUsed, tuneLength = 3, trControl = trainControl)
  pred <<- predict(model, test)
  
  mismatches = (testclass == pred) 
  accuracy <<- sum(mismatches)/length(testclass)
  model
}

# adjust the levels in targets and predictions so that they have the same number of levels
# set all invalid labels to zero in predictions and add a zero factor to both series to get matching level sets
# then call confusion Matrix
getConfusionMatrix <- function(targets, predictions){
  pred <- adjustLevels(targets, predictions)
  targets <- append(targets, c(0))
  pred <- append(pred, c(0))
  confusionMatrix(targets, pred)
}

relabelOutput <- function(data) {
  lables <- levels(as.ordered(data))
  origLabels <- c("A","B","C","D","E")
  out <- factor(glmrOutput, labels = origLabels[1:length(lables)])
  out
}

createModels <- function() {
print("Create Data partitions for training and validation")
set.seed(31415)
training = createDataPartition(trainSet$classe, p = 0.8, list=FALSE)
traindata = trainSet[training,]
testdata = trainSet[-training,]

seeds1 = list(
  c(132, 131, 130), 
  c(155, 154,153), 
  c( 42, 43,44), 
  c( 207, 208 , 209), 
  c( 512, 513,514), 
  c( 133,134,135), 
  c( 412,411,410), 
  c( 232, 231, 230), 
  c( 1602, 1603,1604), 
  c( 2916, 2915, 2914), 
  167)


  

seeds2 = list(
  132:122, 
  155:145, 
  42:52, 
  207:217, 
  512:522, 
  133:143, 
  412:422, 
  232:222, 
  1602:1612, 
  2916:2906, 
  167)

print("Set training control parameters using method 'cv' with 10 folds and sampling freq of 0.75")
trCtrl <- trainControl(method = "cv", number = 10, seeds = seeds2, p = 0.75, returnResamp="all", savePredictions=TRUE)

# generalized linear model using regression
print("Train Generalized linear model using regression.")

glmrConfusion <- getConfusionMatrix(testclass, glmrPreds)
glmrAccuracy <- accuracy
sprintf("Generalized linear model using regression validation accuracy = %f", glmrAccuracy)
glmrPreds <- pred
glmrModel
print("Generalized linear model using regression Confusion Matrix")
print(glmrConfusion)

# generalized linear model using classification
# this model was unable to use classification without throwing an error 
# even though on the caret web page it is listed as a dual purpose method
#print("Train generalized linear model using classification.")
#glmcModel <- trainAndValidateClassification(traindata,testdata, "glm", trCtrl, c("center", "scale", "pca"))
#glmcAccuracy <- accuracy
#glmcPreds <- pred
#glmcConfusion <- getConfusionMatrix(testclass, glmcPreds)
#glmcModel
#glmcConfusion

# K-Nearest Neighbors using regression
print ("Train K-Nearest Neighbors using regression.")
knnrModel <- trainAndValidateRegression(traindata,testdata, "knn", trCtrl, c("center", "scale", "pca"))
knnrConfusion <- getConfusionMatrix(testclass, knnrPreds)
knnrAccuracy <- accuracy
sprintf("K-Nearest Neighbors using regression validation accuracy = %f", knnrAccuracy)
knnrPreds <- pred
knnrModel
print("K-Nearest Neighbors using regression Confusion Matrix")
print(knnrConfusion)

# K-Nearest Neighbors using classification
print ("Train K-Nearest Neighbors using classification")
knncModel <- trainAndValidateClassification(traindata,testdata, "knn", trCtrl, c("center", "scale", "pca"))
knncConfusion <- confusionMatrix(testclass, knncPreds)
knncAccuracy <- accuracy
sprintf("K-Nearest Neighbors using classification validation accuracy  = %f", knncAccuracy)
knncPreds <- pred
knncModel
print("K-Nearest Neighbors using classification Confusion Matrix")
print(knncConfusion)

# Stochastic Gradient Boosting using Regression
print("Train Stochastic Gradient Boosting using Regression.")
gbmrModel <- trainAndValidateRegression(traindata,testdata, "gbm", trCtrl, c("center", "scale", "pca"))
gbmrConfusion <- getConfusionMatrix(testclass, gbmrPreds)
gbmrAccuracy <- accuracy
sprintf("Stochastic Gradient Boosting using Regression validation accuracy = %f", gbmrAccuracy)
gbmrPreds <- pred
gbmrModel
print("Stochastic Gradient Boosting using Regression Confusion Matrix")
print(gbmrConfusion)

# Stochastic Gradient Boosting using Classification
print("Train Stochastic Gradient Boosting using Classification.")
gbmcModel <- trainAndValidateClassification(traindata,testdata, "gbm", trCtrl, c("center", "scale", "pca"))
gbmcConfusion <- confusionMatrix(testclass, gbmcPreds)
gbmcAccuracy <- accuracy
sprintf("Stochastic Gradient Boosting using Classification validation accuracy = %f", gbmcAccuracy)
gbmcPreds <- pred
gbmcModel
print("Stochastic Gradient Boosting using classification Confusion Matrix")
print(gbmcConfusion)


# Support Vector Machines with Radial Basis Function Kernel using regression
print("Train Support Vector Machines with Radial Basis Function Kernel using regression.")
svmrModel <- trainAndValidateRegression(traindata,testdata, "svmRadial", trCtrl, c("center", "scale", "pca"))
svmrConfusion <- getConfusionMatrix(testclass, svmrPreds)
svmrAccuracy <- accuracy
sprintf("Support Vector Machines with Radial Basis Function Kernel using regression validation accuracy = %f", svmrAccuracy)
svmrPreds <- pred
svmrModel
print("Support Vector Machines with Radial Basis Function Kernel using regression Confusion Matrix")
print(svmrConfusion)

# Support Vector Machines with Radial Basis Function Kernel using classification
print("Train Support Vector Machines with Radial Basis Function Kernel using classification.")
svmcModel <- trainAndValidateClassification(traindata,testdata, "svmRadial", trCtrl, c("center", "scale", "pca"))
svmcConfusion <- confusionMatrix(testclass, svmcPreds)
svmcAccuracy <- accuracy
sprintf("Support Vector Machines with Radial Basis Function Kernel using classification validation accuracy = %f", svmcAccuracy)
svmcPreds <- pred
svmcModel
print("Support Vector Machines with Radial Basis Function Kernel using classification Confusion Matrix")
print(svmcConfusion)
}

predictOutcomes <- function() {
  #use various models to classify final test data read from pml-testing.csv
  print("Use trained models to predict output")
  testdata <- cleanData(testSet)
  # remove extraneous column
  td <- testdata[,1:ncol(testdata) - 1]

  # apply regression based models
  glmrOutput <- round(predict(glmrModel, td))
  cat("GLM-Regression output = [", glmrOutput, "]")
  print("")
  knnrOutput <- round(predict(knnrModel, td))
  cat("KNN-Regression output = [", knnrOutput, "]")
  print("")
  gbmrOutput <- round(predict(gbmrModel, td))
  cat("GBM-Regression output = [", gbmrOutput, "]")
  print("")
  svmrOutput <- round(predict(svmrModel, td))
  cat("SVM-Regression output = [", svmrOutput, "]")
  print("")
  #apply classificatipon based models
  knncOutput <- predict(knncModel, td)
  cat("KNN-Classification output = [", as.character(knncOutput), "]")
  print("")
  gbmcOutput <- predict(gbmcModel, td)
  cat("GBM-Classification output = [", as.character(gbmcOutput), "]")
  print("")
  svmcOutput <- predict(svmcModel, td)
  cat("SVM-Classification output = [", as.character(svmcOutput), "]")
  print("")
}






