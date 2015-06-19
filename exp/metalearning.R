runMTLclassify <- function (dsname, metainfo) {
  testIndex <- metainfo$ia[dsname]:metainfo$ib[dsname]
  trainIndex <- -testIndex
  labelIdx <- ncol(metainfo$metabase)
    
  model <- RWeka::J48(class ~ ., metainfo$metabase[trainIndex,])
  preds <- predict(model, metainfo$metabase[testIndex,-labelIdx])
  test <- metainfo$metabase[testIndex,labelIdx]
  measures <- acc.multi.measures(preds, test)
  measures["tests"] <- length(preds)
  
  labels <- change_special_chars(rownames(metainfo$metabase[testIndex,]))
  measures2 <- acc.multi.measures(preds, metainfo$realbest[[dsname]][labels])
    
  #Predict values in multilabel context
  
  
  c(measures, measures2)
}