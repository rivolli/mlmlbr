# R Code
# 12/05/2015
# A. Rivolli, A. C.P.L.F. Carvalho, 2015
# Start the experiment for all datasets

setup = function() {
  files = list.files("exp/", recursive=TRUE, full.name=TRUE);
  for(i in files) source(i);
}

generate_meta = function() {
  setup();
  
  #Generate Metabase default
  metabase <- dataset <- do.call("rbind", lapply(FILES, function(file) {
    path <- get_filenames(file)
    alldata <- read.csv.file(path$get_tempfile('onlyfeatures', '.csv'))
    labels <- rownames(alldata)
    nlabels <- change_special_chars(labels)
    rownames(alldata) <- paste(path$datasetname, nlabels, sep='_')
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
  write.csv(metabase, file='results/000 - metabase.csv')
  
  validation <- c("bibtex", "birds", "CAL500", "corel5k", "emotions", "enron", "flags")
  metabase[,"class"] <- factor(metabase[,"TOP3"])
  
  #Generate targets results
  metrics <- list()
  fmetrics <- data.frame(methodname=character(), metricname=character(), value=numeric())
  for(file in FILES) {
    path <- get_filenames(file)
    if (!path$datasetname %in% validation) break;
    
    cat (" ** ", path$datasetname, " **\n")
    test <- metabase[,"datasetname"] == path$datasetname
    train <- !test
    
    metrics[[path$datasetname]] <- metaclassifier(metabase, train, test)
    for (method in names(metrics[[path$datasetname]])) {
      row <- metrics[[path$datasetname]][[method]]
      for (measure in names(row)) {
        thevalue <- metrics[[path$datasetname]][[method]][measure] * sum(test) / nrow(metabase)
        names(thevalue) <- NULL
        fmetrics <- rbind(fmetrics, data.frame(methodname=method, metricname=measure, value=thevalue))
      }
    }
  }
  
  cat("\n\n---------------------------------------------------------------------\n")
  for (method <- unique(fmetris[,"methodname"])) {
    cat(unique(fmetrics[,"metricname"]),"\n")
    for (measure in unique(fmetrics[,"metricname"])) {
      
    }
  }    
  cat("---------------------------------------------------------------------\n\n\n")
   
  cat("\ndone:", now(), "\n")
  TRUE
}