createMetaModel <- function (metabase, train) {
  nexample <- metabase[,"LRfq"] > 0
  traindata <- metabase[train & nexample,c(getPreditiveAttributes(), "class")]
  #trainSplit <- SMOTE(class ~ ., traindata, perc.over=300, k=3, perc.under=300)
  model <- randomForest(traindata[,-ncol(traindata)], traindata[,ncol(traindata)])
  #model <- svm(traindata[,-ncol(traindata)], traindata[,ncol(traindata)])
  
  model
}

preProccessForMTL <- function (metabase, targetAttribute) {
  #Atr - Number of attributes
#   metabase[metabase[,"Atr"] < 20, "Atr"] <- 0 #very small < 20
#   metabase[metabase[,"Atr"] >= 20 & metabase[,"Atr"] < 100, "Atr"] <- 1 #small < 100
#   metabase[metabase[,"Atr"] >= 100 & metabase[,"Atr"] < 300, "Atr"] <- 2 #medium < 300
#   metabase[metabase[,"Atr"] >= 300 & metabase[,"Atr"] < 900, "Atr"] <- 3 #large < 900
#   metabase[metabase[,"Atr"] >= 900, "Atr"] <- 4 #large < 900

  #metabase[,getPreditiveAttributes()] <- scale(metabase[,getPreditiveAttributes()])
  #metabase[,getPreditiveAttributes()] <- normalize(metabase[,getPreditiveAttributes()])
  metabase[,"LRfq"] <- round(metabase[,"Lfq"] * metabase[,"Spl"])
  metabase[,"class"] <- factor(metabase[,targetAttribute])
  metabase
}

getMetaPredictions <- function (model, metabase, test) {
  testdata <- metabase[test,c(getPreditiveAttributes(), "class")]
  preds <- predict(model, testdata[,-ncol(testdata)], type="class")
  measures <- acc.multi.measures(preds, testdata[,ncol(testdata)])
  
  majority <- acc.multi.measures(factor(rep("SVM", nrow(testdata)), levels=levels(testdata[,ncol(testdata)])), testdata[,ncol(testdata)])
  measures["majority"] <- majority["accuracy"]
  measures["majorityf"] <- majority["fscore"]
  
  #Ballanced Accuracy
  res <- confusionMatrix(preds, testdata[,ncol(testdata)])
  measures["balancedaccuracy"] <- sum(res$byClass[!is.na(res$byClass[,"Balanced Accuracy"]),"Balanced Accuracy"]) / nlevels(testdata[,ncol(testdata)])
  
  measures["examples"] <- sum(test)
  
  return(list(metrics=measures, predictions=preds))
}

getMultilabelResults <- function (metaprediction) {
  path <- attr(metaprediction, "path")
  cat(" ** ", path$datasetname, " **\n")
  testdata <- mldr(path$testfile, auto_extension=FALSE, xml_file=path$xmlfile)
  for (method in c('SVM', 'NB', "RF", "DT")) {
    load(path$get_tempfile(method, '.RData'))
  }
  lresults <- list(SVM=svm.results, NB=nb.results, RF=rf.results, DT=dt.results)
  for (knn in c("KNN_1", "KNN_3", "KNN_5", "KNN_7")) {
    load(path$get_tempfile(knn, '.RData'))
    lresults[[knn]] <- knn.results
  }
  
  classifiers <- as.character(metaprediction)
  names(classifiers) <- gsub(paste(path$datasetname, '_', sep=''), '', names(metaprediction))
  predictions <- get_predictions_from_list(classifiers, lresults, testdata)
  
  mlresults <- BR.evaluate(testdata, predictions)
  mresult.as.vector(mlresults)
}

getPreditiveAttributes <- function () {
  c(
    "Atr", "Dim", "Spl","Lfq"
    ,"NumRate", "NomRate","SymMin", "SymMax", "SymMean", "SymSd", "SymSum" 
    ,"Sks", "SksP", "Kts", "KtsP", "AbsC", "AtrEnt"
    ,"CanC", "ClEnt", "NClEnt", "JEnt", "MutInf", "EAttr", "NoiSig"
  )
}