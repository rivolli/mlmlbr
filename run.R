# R Code
# 12/05/2015
# A. Rivolli, A. C.P.L.F. Carvalho, 2015
# Start the experiment for all datasets

setup = function() {
  files = list.files("exp/", recursive=TRUE, full.name=TRUE);
  for(i in files) source(i);
}

run = function() {
  setup();
  
  results <- lapply(FILES, function(file) {
    root(file);
    
#     #Generate dataset info
#     path <- get_filenames(file)
#     traindata <- mldr(path$trainfile, auto_extension=FALSE, xml_file=path$xmlfile)
#     testdata <- mldr(path$testfile, auto_extension=FALSE, xml_file=path$xmlfile)
#     
#     list(
#       name=path$datasetname,
#       train=traindata$measures$num.instances,
#       test=testdata$measures$num.instances,
#       attr=traindata$measures$num.attributes,
#       labels=traindata$measures$num.labels,
#       lcard=traindata$measures$cardinality,
#       lden=traindata$measures$density,
#       labelset=traindata$measures$num.labelsets,
#       singlelabelset=traindata$measures$num.single.labelsets
#     )
  });
  
  cat("\ndone:", now(), "\n")
  TRUE
}

run();