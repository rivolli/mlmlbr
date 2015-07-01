createMetaModel <- function (metabase, train) {
  traindata <- metabase[train,c(getPreditiveAttributes(), "class")]
  model <- randomForest(traindata[,-ncol(traindata)], traindata[,ncol(traindata)])
  
  model
}

getMetaPredictions <- function (model, metabase, test) {
  testdata <- metabase[test,c(getPreditiveAttributes(), "class")]
  preds <- predict(model, testdata[,-ncol(testdata)])
  measures <- acc.multi.measures(preds, testdata[,ncol(testdata)])
  reals <- table(testdata[,ncol(testdata)])
  measures["majority"] <- reals[which.max(reals)] / sum(test)
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
    "Atr", "Dim"
    ,"ClMin", "ClMax", "ClSd"
    ,"Sks", "SksP", "Kts", "KtsP", "AbsC"
    ,"CanC", "ClEnt", "NClEnt", "JEnt", "MutInf", "EAttr", "NoiSig"
    #,"Lfq", "IRLbl", "LScl"
    ,"Nlbst", "NSlbst", "Mfreq", "LCard", "LDen", "Mir", "Scl"
    #, "AtrEnt", "NAtrEnt"
    #,"NumRate", "NomRate","SymMin", "SymMax", "SymMean", "SymSd", "SymSum"
  )
} 

metaclassifier <- function (metabase, train, test) {
  #Decision tree
  
  
#   scaleddata <- cbind(as.data.frame(scale(metabase[,c(getPreditiveAttributes())])), metabase[,"class"])
#   scaledtrain <- scaleddata[train,]
#   scaledtest <-  scaleddata[test,]
  lbl <- ncol(traindata)
  
#   model1 <- RWeka::J48(class ~ ., traindata)
#   preds1 <- predict(model1, testdata[,-lbl])
#   measu1 <- acc.multi.measures(preds1, testdata[,lbl])
#   cat(" DT", measu1, "\n")
  
  model2 <- randomForest(traindata[,-lbl], traindata[,lbl])
  preds2 <- predict(model2, testdata[,-lbl])
  measu2 <- acc.multi.measures(preds2, testdata[,lbl])
  cat(" RF", measu2, "\n")
  
#   model3 <- svm(traindata[,-lbl], traindata[,lbl])
#   preds3 <- predict(model3, testdata[,-lbl])
#   measu3 <- acc.multi.measures(preds3, testdata[,lbl])
#   cat("SVM", measu3, "\n")
#   
#   model4 <- naiveBayes(traindata[,-lbl], traindata[,lbl])
#   preds4 <- predict(model4, testdata[,-lbl])
#   measu4 <- acc.multi.measures(preds4, testdata[,lbl])
#   cat(" NB", measu4, "\n")
#   
#   preds5 <- knn(scaledtrain[,-lbl], scaledtest[,-lbl], scaledtrain[,lbl], 5)
#   measu5 <- acc.multi.measures(preds5, scaledtest[,lbl])
#   cat("KNN", measu5, "\n")
  
#   allpreds <- cbind(
#     as.character(preds1), #DT
#     as.character(preds2), #RF
#     as.character(preds3), #SVM
#     as.character(preds4), #NB
#     as.character(preds5)  #KNN
#   )
#   preds0 <- factor(apply(allpreds, 1, function (row) names(which.max(table(row)))), levels=unique(metabase[,"class"]))
#   measu0 <- acc.multi.measures(preds0, testdata[,lbl])
#   cat("ENS", measu0, "\n")
#   
#   return (list(
#     metrics=list(DT=measu1, RF=measu2, SVM=measu3, NB=measu4, KNN=measu5, ENS=measu0),
#     prediciton=list(DT=preds1, RF=preds2, SVM=preds3, NB=preds4, KNN=preds5, ENS=preds0)
#   ))
}
