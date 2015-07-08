# R Code
# 12/05/2015
# A. Rivolli, A. C.P.L.F. Carvalho, 2015
# Start the experiment for all datasets

setup = function() {
  files = list.files("exp/", recursive=TRUE, full.name=TRUE);
  for(i in files) source(i);
}

generate_meta = function() {
  mlmetrics <- c("Accuracy", "AUC", "Recall", "Precision", "AveragePrecision", "FMeasure", "HammingLoss", "SubsetAccuracy", "MacroFMeasure", "MicroFMeasure")
  
  #Generate Metabase default
  metabase <- do.call("rbind", lapply(FILES, function(file) {
    path <- get_filenames(file)
    alldata <- read.csv.file(path$get_tempfile('onlyfeatures', '.csv'))
    labels <- rownames(alldata)
    nlabels <- change_special_chars(labels)
    rownames(alldata) <- paste(path$datasetname, nlabels, sep='_')
    alldata <- as.data.frame(apply(alldata, 2, function (col) { if (sum(is.na(col)) > 0) col[is.na(col)] <- 0; col; }))
    alldata[,"datasetname"] <- path$datasetname
    alldata[,"labelname"] <- labels
    
    #TOP3
    load(path$get_tempfile('TOP3classifiers', '.RData')) #classifiers
    alldata[,"TOP3"] <- classifiers[nlabels]
    
    #ALL
    load(path$get_tempfile('ALLclassifiers', '.RData')) #classifiers
    alldata[,"ALL"] <- classifiers[nlabels]
    
    alldata
  }));
  datasetnames <- unique(metabase[,"datasetname"])
  write.csv(metabase, file='results/000 - metabase.csv')
  metabase <- preProccessForMTL(metabase, "TOP3")
  write.csv(metabase[,c(getPreditiveAttributes(), "class")], file='results/001 - metabase.csv')
  
  write.table(table(metabase[,"class"]), quote = F, row.names=F, col.names=F)
  cat("\n")
  
  #Generate class attribute
  metrics <- list()
  predictions <- list()
  svmmlresult <- list()
  
  validationIndex <- 2:(length(FILES)+1)
  validationIndex[length(validationIndex)] <- 1
  
  for(f in 1:length(FILES)){
    pathTest <- get_filenames(FILES[f])
    pathValidation <- get_filenames(FILES[validationIndex[f]])
    
    cat (" ** ", pathValidation$datasetname, pathTest$datasetname, " **\n")
    test <- metabase[,"datasetname"] == pathTest$datasetname
    validation <- metabase[,"datasetname"] == pathValidation$datasetname
    train <- !test & !validation
    
    metaModel <- createMetaModel(metabase, train);
    res <- getMetaPredictions(metaModel, metabase, validation)
    metrics[[pathValidation$datasetname]] <- res[["metrics"]]
    predictions[[pathValidation$datasetname]] <- res[["predictions"]]
    attr(predictions[[pathValidation$datasetname]], "path") <- pathValidation
    
    svmmlresult[[pathValidation$datasetname]] <- read.csv.file(pathValidation$datasetinfo)["SVM", mlmetrics]
  }
  allresults <- do.call("rbind", metrics)
  
  cat("\n\n---------------------------------------------------------------------\n")
  metrics <- c("error", "precision", "recall", "fscore", "accuracy", "balancedaccuracy", "auc", "majority")
  total <- apply(allresults[, metrics] * allresults[,"examples"], 2, sum) / sum(allresults[, "examples"])
  total["examples"] <- sum(allresults[, "examples"])
  allresults <- rbind(allresults, total)
  write.csv(allresults, file="meta-learning-metrics.csv")

  cat("Mean of metrics:", paste(paste("\n", names(total)), total, sep=": "), "\n")
  browser()
  #Multi-Label results
  mlresults <- do.call("rbind", lapply(predictions, getMultilabelResults))
  total <- apply(mlresults[,mlmetrics], 2, mean)
  mlresults <- rbind(mlresults[,mlmetrics], total)
  
  svmresult <- do.call("rbind", svmmlresult)
  total <- apply(svmresult[,mlmetrics], 2, mean)
  compsvmresult <- rbind(svmresult, total)
  
  write.csv(cbind(allresults, (mlresults - compsvmresult), compsvmresult), file="meta-learning-metrics.csv")

  statisticalResult <- data.frame(observation=character(), wins=integer(), loss=integer(), svm=numeric(), mtl=numeric(), relevance=logical())
  for (measure in mlmetrics) {
    valid <- wilcoxon(svmresult[datasetnames, measure], mlresults[datasetnames, measure], .95)
    diffs <- round(mlresults[datasetnames, measure] - svmresult[datasetnames, measure], 2)
    statisticalResult <- rbind(statisticalResult, 
      list(observation=as.character(measure), wins=sum(diffs > 0), loss=sum(diffs < 0),
                svm=total[measure], mtl=mlresults["total", measure], relevance=valid)
    )
    cat(measure, "SVM:", total[measure], " |  MTL:",mlresults["total", measure], " | Different:", valid,"\n")
  }
  
  #change hamming loss values
  svmresult[,"HammingLoss"] <- 1 - svmresult[,"HammingLoss"]
  mlresults[,"HammingLoss"] <- 1 - mlresults[,"HammingLoss"]
  cat("\n")
  for (dsname in datasetnames) {
    valid <- wilcoxon(as.numeric(svmresult[dsname,]), as.numeric(mlresults[dsname,]), .95)
    diffs <- round(mlresults[dsname,] - svmresult[dsname,], 2)
    statisticalResult <- rbind(statisticalResult, list(observation=as.character(dsname), wins=sum(diffs > 0), loss=sum(diffs < 0),svm=0, mtl=0, relevance=valid))
    cat(dsname, "SVM:", sum(diffs < 0), 
        " wins |  MTL:", sum(diffs > 0), " wins | Different:", valid,"\n")
  }
  write.csv(statisticalResult, file="meta-learning-statistical.csv")
  browser()
  
  cat("\ndone:", now(), "\n")
  TRUE
}