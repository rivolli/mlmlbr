runMTLclassify <- function (dsname, metainfo) {
  cat("** ", dsname, " **\n")
  testIndex <- metainfo$ia[dsname]:metainfo$ib[dsname]
  trainIndex <- -testIndex
  labelIdx <- ncol(metainfo$metabase)
  
  #Decision tree
  model <- RWeka::J48(class ~ ., metainfo$metabase[trainIndex,])
  preds <- predict(model, metainfo$metabase[testIndex,-labelIdx])
  measures <- acc.multi.measures(preds, metainfo$metabase[testIndex,labelIdx])
  
  #   class <- metainfo$metabase[,labelIdx]
  #   svmData <- scale(metainfo$metabase[,-labelIdx])
  #   metabase <- cbind(as.data.frame(svmData), class)
  #   smodel <- svm(svmData[trainIndex,], metabase[trainIndex,ncol(metabase)])
  #   npreds <- predict(smodel, svmData[testIndex,])
  #   measures <- acc.multi.measures(npreds, metabase[testIndex,labelIdx])
  
  measures["tests"] <- length(preds)
  
  orignlabels <- gsub(paste(dsname, '_', sep=''), '', rownames(metainfo$metabase[testIndex,]))
  labels <- change_special_chars(orignlabels)
  measures2 <- acc.multi.measures(preds, metainfo$realbest[[dsname]][labels])
  
  #Save predicted classifiers
  path <- get_filenames(paste('dataset/', dsname, '/', dsname, '-train.arff', sep=''))
  if (length(metainfo$realbest[[dsname]]) != length(preds)) {
    alldata <- read.csv.file(path$get_tempfile('onlyfeatures', '.csv'))[,colnames(metainfo$metabase[testIndex,-labelIdx])]
    allpreds <- predict(model, alldata)
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