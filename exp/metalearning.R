runMTLclassify <- function (dsname, metainfo) {
  cat("** ", dsname, " **\n")
  testIndex <- metainfo$ia[dsname]:metainfo$ib[dsname]
  trainIndex <- -testIndex
  labelIdx <- ncol(metainfo$metabase)
    
  model <- RWeka::J48(class ~ ., metainfo$metabase[trainIndex,])
  preds <- predict(model, metainfo$metabase[testIndex,-labelIdx])
  measures <- acc.multi.measures(preds, metainfo$metabase[testIndex,labelIdx])
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
  
  save(allpreds, file=path$get_tempfile('allprediction', '.RData'))
  
  c(measures, measures2)
}

evaluatingMultiLabel <- function (preds) {
  
  
}