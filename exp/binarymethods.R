get_baseline_measures <- function (trainData, testData, labelIdx) {
  baseline.result <- rep(as.numeric(names(which.max(table(trainData[,labelIdx])))), nrow(testData))
  baseline.measures <- binary.evaluate.complete(baseline.result, testData[,labelIdx], baseline.result)
  
  baseline.measures
}

get_knn_measures <- function (trainData, testData, labelIdx, k) {
  knn.result <- knn(trainData[,-labelIdx], testData[,-labelIdx], trainData[,labelIdx], k, prob=T)
  knn.probs <- ifelse(knn.result == 0, 1-attr(knn.result, "prob"), attr(knn.result, "prob"))
  knn.measures <- binary.evaluate.complete(knn.result, testData[,labelIdx], knn.probs)
  
  knn.measures
}

get_svm_measures <- function (trainData, testData, labelIdx, testIdx) {
  svmData <- as.matrix(trainData[,-labelIdx])
  rownames(svmData) <- NULL

  if (sum(table(trainData[,labelIdx]) > 0) >= 1) {
    #Invalid result in ksvm: Nos proximos experimentos nao utilizar o ksvm
    svm.model <- svm(svmData, trainData[,labelIdx], probability=T)
    svm.result <- predict(svm.model, testData[,-labelIdx], probability = T)
    svm.probs <- attr(svm.result, "probabilities")[,2]
    rm(svm.model)
  }
  else {
    #Invalid result to ksvm and svm so complete result with 0 values
    cat("________ Invalid traindata to SVM classifier ________\n")
    svm.result <- rep(2, length(testIdx)) #All values are wrong
    svm.probs <- rep(0, length(testIdx)) #All probabilites are 0
  }
  svm.measures <- binary.evaluate.complete(svm.result, testData[,labelIdx], svm.probs)
  
  svm.measures
}

get_naivebayes_measures <- function (trainData, testData, labelIdx) {
  nb.model <- naiveBayes(trainData[,-labelIdx], trainData[,labelIdx], type="raw")
  nb.probs <- predict(nb.model, testData[,-labelIdx], "raw")[,2]
  nb.result <- as.numeric(nb.probs>0.5)
  
  nb.measures <- binary.evaluate.complete(nb.result, testData[,labelIdx], nb.probs)
  
  nb.measures
}

get_decisiontree_measures <- function (trainData, testData, labelIdx, classname) {
  formula <- as.formula(paste(classname, " ~ .", sep=""))
  dt.model <- RWeka::J48(formula, trainData) #Using RWeka:: prefix because whithout it, Running with mclapply doesnt work
  dt.probs <- predict(dt.model, testData[,-labelIdx], "probability")[,2]
  dt.result <- as.numeric(dt.probs>0.5)
  dt.measures <- binary.evaluate.complete(dt.result, testData[,labelIdx], dt.probs)
  rm(formula, dt.model, dt.probs, dt.result)
  
  dt.measures
}

get_randomforest_measures <- function (trainData, testData, labelIdx, testIdx) {
  if (sum(table(trainData[,labelIdx]) > 0) >= 1) {
    rf.model <- randomForest(trainData[,-labelIdx], trainData[,labelIdx])
    rf.probs <- predict(rf.model, testData[,-labelIdx], "prob")[,2]
    rf.result <- as.numeric(rf.probs>0.5)
    rm(rf.model)
  }
  else {
    #Invalid result to randomForest so complete result with 0 values
    cat("________ Invalid traindata to randomForest classifier ________\n")
    rf.result <- rep(2, length(testIdx)) #All values are wrong
    rf.probs <- rep(0, length(testIdx)) #All probabilites are 0
  }
  rf.measures <- binary.evaluate.complete(rf.result, testData[,labelIdx], rf.probs)
  
  rf.measures
}