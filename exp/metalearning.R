runMTLclassify <- function (dsname, metainfo) {
  testIndex <- metainfo$ia[dsname]:metainfo$ib[dsname]
  trainIndex <- -testIndex
  labelIdx <- ncol(metainfo$metabase)
  traindata <- metainfo$metabase[trainIndex,-labelIdx] #Remove last column that is the real better classifiers
  labelIdx <- labelIdx - 1
  
  model <- RWeka::J48(class ~ ., traindata)
  preds <- predict(model, traindata[testIndex,-labelIdx])
  measures <- acc.multi.measures(preds, metainfo$metabase[testIndex,labelIdx])
  measures["tests"] <- length(preds)
  
  realmeasures <- acc.multi.measures(preds, metainfo$metabase[testIndex,(labelIdx + 1)])
  
  cat(dsname, measures, realmeasures, "\n")
  browser()
}