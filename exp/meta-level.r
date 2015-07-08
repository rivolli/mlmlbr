metaclassifier <- function (metabase, train, test) {
  #Decision tree
  traindata <- metabase[train,c(getPreditiveAttributes(), "class")]
  testdata <- metabase[test,c(getPreditiveAttributes(), "class")]
  lbl <- ncol(traindata)
  
  model1 <- RWeka::J48(class ~ ., traindata)
  preds1 <- predict(model1, testdata[,-lbl])
  measu1 <- acc.multi.measures(preds1, testdata[,lbl])
  cat(" DT", measu1, "\n")
  
  model2 <- randomForest(traindata[,-lbl], traindata[,lbl])
  preds2 <- predict(model2, testdata[,-lbl])
  measu2 <- acc.multi.measures(preds2, testdata[,lbl])
  cat(" RF", measu2, "\n")
  
  model3 <- svm(traindata[,-lbl], traindata[,lbl])
  preds3 <- predict(model3, testdata[,-lbl])
  measu3 <- acc.multi.measures(preds3, testdata[,lbl])
  cat("SVM", measu3, "\n")
  
  model4 <- naiveBayes(traindata[,-lbl], traindata[,lbl])
  preds4 <- predict(model4, testdata[,-lbl])
  measu4 <- acc.multi.measures(preds4, testdata[,lbl])
  cat(" NB", measu4, "\n")
  
  preds5 <- knn(traindata[,-lbl], testdata[,-lbl], traindata[,lbl], 1)
  measu5 <- acc.multi.measures(preds5, testdata[,lbl])
  cat("KNN", measu5, "\n")
  
  allpreds <- cbind(
    as.character(preds1),
    as.character(preds2),
    as.character(preds3),
    as.character(preds4),
    as.character(preds5)
  )
  preds0 <- factor(apply(allpreds, 1, function (row) names(which.max(table(row)))), levels=unique(metabase[,"class"]))
  measu0 <- acc.multi.measures(preds0, testdata[,lbl])
  cat("ENS", measu0, "\n")
  
  return (list(DT=measu1, RF=measu2, SVM=measu3, NB=measu4, KNN=measu5, ENS=measu0))
}

getPreditiveAttributes <- function () {
  c("Atr", "Dim", "NumRate", "NomRate", "Sks", "SksP", "Kts", "KtsP", "AbsC",
    "CanC", "ClEnt", "NClEnt", "JEnt", "MutInf", "EAttr", "NoiSig"
    ,"Lfq"
    #"IRLbl", "LScl"
    #,"LCard", "LDen", "Mir", "Scl"
    #, "AtrEnt", "NAtrEnt"
  )
} 