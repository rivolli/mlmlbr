runningExperimentsEvaluation <- function (traindata, testdata, path) {
  #SVM Result
  resultfile <- path$get_tempfile('SVM', '.RData')
  if (file.exists(resultfile)) {
    cat (now(), "Loading SVM\n")
    load(resultfile)
  }
  else {
    cat (now(), "Running SVM\n")
    svm.results <- BinaryRelevance(traindata, testdata, "SVM", CORES)  
    save(svm.results, file=resultfile)
  }
  
  #NB Result
  resultfile <- path$get_tempfile('NB', '.RData')
  if (file.exists(resultfile)) {
    cat (now(), "Loading NB\n")
    load(resultfile)
  }
  else {
    cat (now(), "Running NB\n")
    nb.results <- BinaryRelevance(traindata, testdata, "NB", CORES)
    save(nb.results, file=resultfile)
  }
  
  #RF Result
  resultfile <- path$get_tempfile('RF', '.RData')
  if (file.exists(resultfile)) {
    cat (now(), "Loading RF\n")
    load(resultfile)
  }
  else {
    cat (now(), "Running RF\n")
    rf.results <- BinaryRelevance(traindata, testdata, "RF", CORES)
    save(rf.results, file=resultfile)
  }
  
  #DT Result
  resultfile <- path$get_tempfile('DT', '.RData')
  if (file.exists(resultfile)) {
    cat (now(), "Loading DT\n")
    load(resultfile)
  }
  else {
    cat (now(), "Running DT\n")
    dt.results <- BinaryRelevance(traindata, testdata, "DT", CORES)
    save(dt.results, file=resultfile)
  }
  
  lresults <- list("SVM"=svm.results, "NB"=nb.results, "RF"=rf.results, "DT"=dt.results)
  
  #KNNs
  #Running KNN 1, 3, 5, 7
  for (k in seq(1, 7, 2)) {
    methodname <- paste('KNN', k, sep='_')
    resultfile <- path$get_tempfile(methodname, '.RData')
    if (file.exists(resultfile)) {
      cat (now(), "Loading", methodname, "\n")
      load(resultfile)
    }
    else {
      cat(now(), "Running", methodname, "\n")
      knn.results <- BinaryRelevance(traindata, testdata, methodname, CORES)
      save(knn.results, file=resultfile)
    }
    
    lresults[[methodname]] <- knn.results
    rm(knn.results)
  }
  
  datasetresult <- read.csv.file(path$resultfile)
  
  #All Better Result AUC
  cat (now(), "Running AUC\n")
  predictions <- get_predictions_from_csv(datasetresult, lresults, testdata, "topauc");
  lresults[["AUC"]] <- BR.evaluate(testdata, predictions)
  
  #All Better Result ACC
  cat (now(), "Running ACC\n")
  predictions <- get_predictions_from_csv(datasetresult, lresults, testdata, "topaccuracy");
  lresults[["ACC"]] <- BR.evaluate(testdata, predictions)

  #Random Result (only TOP3 classifiers)
  resultfile <- path$get_tempfile('RAND', '.RData')
  if (file.exists(resultfile)) {
    cat (now(), "Loading RANDOM\n")
    load(resultfile)
  }
  else {
    cat (now(), "Running RANDOM\n")
    totals <- matrix(nrow = 10, ncol = 19)
    for (i in 1:10) { 
      #Running 10 times and use the mean of metrics
      classifiers <- sapply(rownames(testdata$labels), function (j) sample(c("SVM", "RF", "NB"))[1])
      predictions <- get_predictions_from_list(classifiers, lresults, testdata)
      random.results <- BR.evaluate(testdata, predictions)
      totals[i,] <- mresult.as.vector(random.results)
    }
    mrandom <- apply(totals, 2, mean)
    for (i in 1:length(mrandom)) random.results[[i]] <- mrandom[i]
    save(random.results, file=resultfile)
  }
  lresults[["RANDOM"]] <- random.results
  
  #Prediction Result
  file=path$get_tempfile('allprediction', '.RData')
  if (file.exists(file)) {
    cat (now(), "Running PREDICTIONS\n")
    load(file) #allpreds
    names(allpreds) <- change_special_chars(names(allpreds))
    predictions <- get_predictions_from_list(allpreds, lresults, testdata)
    lresults[["PRED"]] <- BR.evaluate(testdata, predictions)
  }
  
  #All Better REAL Result in TOP3
  cat (now(), "Running TOP3\n")
  classifiers <- get_betters_classifiers(list("SVM"=svm.results, "NB"=nb.results, "RF"=rf.results), testdata)
  predictions <- get_predictions_from_list(classifiers, lresults, testdata)
  lresults[["TOP3"]] <- BR.evaluate(testdata, predictions)
  save(classifiers, file=path$get_tempfile('TOP3classifiers', '.RData'))
  
  #All Better Result in All classifiers
  cat (now(), "Running ALL\n")
  predictions <- get_predictions_from_csv(NULL, lresults, testdata)
  lresults[["ALL"]] <- BR.evaluate(testdata, predictions)

  content <- do.call(rbind, lapply(lresults, mresult.as.vector))
}

get_predictions_from_csv <- function (datasetresult, lresults, testdata, attrname="") {
  if (is.null(datasetresult)) {
    classesresult <- c()
  }
  else {
    classesresult <- rownames(datasetresult)
  }
  
  predictions <- matrix(nrow=testdata$measures$num.instances, ncol=testdata$measures$num.labels)
  colnames(predictions) <- rownames(testdata$labels)

  for (classname in rownames(testdata$labels)) {
    if (classname %in% classesresult) {
      #Alread calculated what is the best method
      method <- as.character(datasetresult[classname, attrname])
    }
    else {
      #Use the best method observed in the results
      results <- lapply(lresults, function (res) sum(attr(res, "predictions")[,classname] == testdata$dataset[,classname]))
      method <- names(which.max(results))
    }
    predictions[,classname] <- attr(lresults[[method]], "predictions")[,classname]
  }
  
  predictions
}

get_predictions_from_list <- function (classifiers, lresults, testdata) {
  predictions <- matrix(nrow=testdata$measures$num.instances, ncol=testdata$measures$num.labels)
  colnames(predictions) <- rownames(testdata$labels)
  for (classname in rownames(testdata$labels)) {
    if (is.na(classifiers[classname])) {
      predictions[,classname] <- rep(0, testdata$measures$num.instances)
    }
    else {
      predictions[,classname] <- attr(lresults[[classifiers[classname]]], "predictions")[,classname]  
    }
  }
  predictions
}

get_betters_classifiers <- function (lresults, testdata) {
  classifiers <- c()
  for (classname in rownames(testdata$labels)) {
    results <- lapply(lresults, function (res) sum(attr(res, "predictions")[,classname] == testdata$dataset[,classname]))
    classifiers[classname] <- names(which.max(results))
  }
  classifiers
}
