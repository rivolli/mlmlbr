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
    rownames(alldata) <- paste(path$datasetname, change_special_chars(labels), sep='_')
    alldata[,"datasetname"] <- path$datasetname
    alldata[,"labelname"] <- labels
    alldata
  }));
  write.csv(metabase, file='results/000 - metabase.csv')
  
  #Generate targets results
  lapply(FILES, function(file) {
    path <- get_filenames(file)
    load(path$get_rdatafile('details')) #results
    
#     labelnames <- change_special_chars(rownames(read.csv.file(path$resultfile)))
#     nresults <- lapply(results, function (item) {
#       N <- 100
#       for (i in 1:10) {
#         item$BASELINE[i, "Accuracy"] * N
#       }
#     })
  })
  
  cat("\ndone:", now(), "\n")
  TRUE
}