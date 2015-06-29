runMTLclassify <- function (dsname, metainfo) {
  cat("** ", dsname, " **\n")
  testIndex <- metainfo$ia[dsname]:metainfo$ib[dsname]
  trainIndex <- -testIndex
  labelIdx <- ncol(metainfo$metabase)
  
  #Decision tree
  model <- RWeka::J48(class ~ ., metainfo$metabase[trainIndex,])
  preds <- predict(model, metainfo$metabase[testIndex,-labelIdx])
  measures <- acc.multi.measures(preds, metainfo$metabase[testIndex,labelIdx])
  
  debug <- list(DT=1 - measures["error"])
  
  #ENSEMBLE OF CLASSIFIERS FOR METALEARNING
   #Random Forest
   model2 <- randomForest(metainfo$metabase[trainIndex,-labelIdx], metainfo$metabase[trainIndex,labelIdx])
   preds2 <- predict(model2, metainfo$metabase[testIndex,-labelIdx])
   debug[["RF"]] <- 1 - acc.multi.measures(preds2, metainfo$metabase[testIndex,labelIdx])["error"]
   
   #SVM
   class <- metainfo$metabase[,labelIdx]
   model3 <- svm(metainfo$metabase[trainIndex,-labelIdx], metainfo$metabase[trainIndex,labelIdx], kernel="polynomial", degree=5)
   preds3 <- predict(model3, metainfo$metabase[testIndex,-labelIdx])
   debug[["SVM"]] <- 1 - acc.multi.measures(preds3, metainfo$metabase[testIndex,labelIdx])["error"]
   
   #KNN 5
   scaleData <- scale(metainfo$metabase[,-labelIdx])
   metabase <- cbind(as.data.frame(scaleData), class)
   preds4 <- knn(metainfo$metabase[trainIndex,-labelIdx], metainfo$metabase[testIndex,-labelIdx], metainfo$metabase[trainIndex,labelIdx], 26)
   debug[["KNN"]] <- 1 - acc.multi.measures(preds4, metainfo$metabase[testIndex,labelIdx])["error"]
  
   #NB
   model1 <- naiveBayes(metainfo$metabase[trainIndex,-labelIdx], metainfo$metabase[trainIndex,labelIdx], laplace=2)
   preds1 <- predict(model1, metainfo$metabase[testIndex,-labelIdx])
   debug[["NB"]] <- 1 - acc.multi.measures(preds1, metainfo$metabase[testIndex,labelIdx])["error"]
  
   cat(dsname, unlist(debug), "\n")
   
   allpreds <- cbind(as.character(preds), as.character(preds1), as.character(preds2), as.character(preds3), as.character(preds4))
   allpreds <- cbind(as.character(preds1), as.character(preds2), as.character(preds3), as.character(preds4))
   lclassifiers <- c("SVM", "NB", "RF") #, "KNN_3"
   predsensemble <- factor(apply(allpreds, 1, function (row) names(which.max(table(row)))), levels=lclassifiers)
   measures <- acc.multi.measures(predsensemble, metabase[testIndex,labelIdx])
  #measures <- acc.multi.measures(preds, metabase[testIndex,labelIdx])
  measures["tests"] <- length(preds)
  
  orignlabels <- gsub(paste(dsname, '_', sep=''), '', rownames(metainfo$metabase[testIndex,]))
  labels <- change_special_chars(orignlabels)
  measures2 <- acc.multi.measures(preds, metainfo$realbest[[dsname]][labels])
  
  #Save predicted classifiers
  path <- get_filenames(paste('dataset/', dsname, '/', dsname, '-train.arff', sep=''))
  if (length(metainfo$realbest[[dsname]]) != length(preds)) {
    alldata <- read.csv.file(path$get_tempfile('onlyfeatures', '.csv'))[,colnames(metainfo$metabase[testIndex,-labelIdx])]
    #allpreds <- predict(model, alldata)
    #names(allpreds) <- rownames(alldata)
    preds <- predict(model, alldata)
    preds2 <- predict(model2, alldata)
    preds3 <- predict(model3, alldata)
    preds4 <- knn(metainfo$metabase[trainIndex,-labelIdx], alldata, metainfo$metabase[trainIndex,labelIdx], 26)
    preds1 <- predict(model1, alldata)
    allpreds <- cbind(as.character(preds), as.character(preds1), as.character(preds2), as.character(preds3), as.character(preds4))
    allpreds <- factor(apply(allpreds, 1, function (row) names(which.max(table(row)))), levels=lclassifiers)
    names(allpreds) <- rownames(alldata)
    
    measures2 <- acc.multi.measures(allpreds, metainfo$realbest[[dsname]][change_special_chars(rownames(alldata))])    
  }
  else {
    allpreds <- preds
    names(allpreds) <- orignlabels
  }
  #evaluatingMultiLabel(path, allpreds)
  save(allpreds, file=path$get_tempfile('allprediction', '.RData'))
  
  c(measures, measures2)
}

evaluatingMultiLabel <- function (path, allpreds) {
  cat(" - Reading testfile\n")
  testdata <- mldr(path$testfile, auto_extension=FALSE, xml_file=path$xmlfile)
  cat(" - Reading methods results\n")
  for (method in c('SVM', 'NB', "RF", "DT")) {
    resultfile <- path$get_tempfile(method, '.RData')
    load(resultfile)
  }
  lresults <- list(
    SVM=svm.results,
    NB=nb.results,
    RF=rf.results,
    DT=dt.results
  )
  for (knn in c("KNN_1", "KNN_3", "KNN_5", "KNN_7")) {
    resultfile <- path$get_tempfile(knn, '.RData')
    load(resultfile)
    lresults[[knn]] <- knn.results
  }
  
  cat(" - Predicting and evaluating\n")
  names(allpreds) <- change_special_chars(names(allpreds))
  predictions <- get_predictions_from_list(allpreds, lresults, testdata)
  pred.results <- BR.evaluate(testdata, predictions)
  
  cat(" - Reading and saving INFODATA\n")
  infodata <- read.csv.file(path$datasetinfo)
  infodata["PRED",] <- mresult.as.vector(pred.results)
  write.csv(infodata, file=path$datasetinfo)
}