runMTLclassify <- function (dsname, metainfo) {
  testIndex <- metainfo$ia[dsname]:metainfo$ib[dsname]
  trainIndex <- -testIndex
  labelIdx <- ncol(metainfo$metabase)
  
  model <- RWeka::J48(class ~ ., metainfo$metabase[trainIndex,])
  preds <- predict(model, metainfo$metabase[testIndex,-labelIdx])
  test <- metainfo$metabase[testIndex,labelIdx]
  measures <- acc.multi.measures(preds, test)
  measures["tests"] <- length(preds)
  
  cat(dsname, measures, "\n")
  browser()
}