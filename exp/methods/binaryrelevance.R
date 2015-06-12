# Binary Relevance Method
# 10/06/2015
# A. Rivolli, A. C.P.L.F. Carvalho, 2015

#' @title Binary Relevance Method
#' @param mtraindata mldr dataset with train instances
#' @param mtestdata mldr data with test instances
#' @param method String or list of string with methods names.
#'    If method is a vector the length(method) must be equals the number of class
#'    The suportected methods is 'SVM', 'RF', 'NB'. The Default is SVM
#' @param cores Number of cores to running in parallel
BinaryRelevance <- function (mtraindata, mtestdata, method="SVM", cores=1) {
  if (length(method) != mtraindata$measures$num.labels) {
    method <- rep(method[1], mtraindata$measures$num.labels)
  }

  #Break in L datasets for Binary Relevance
  datasets <- lapply(mldr_transform(mtraindata), function (df) {
    #Convert the class column as factor
    df[,ncol(df)] <- as.factor(df[,ncol(df)])
    #Remove .labelcount and .SCUMBLE
    df[!(names(df) %in% c(".labelcount", ".SCUMBLE"))]
  })
  
  binary.result <- {}
  if (cores == 1) {
    binary.result <- lapply(datasets, function (ds) BR.run(ds, method[substitute(ds)[[3]]], mtestdata))
  }
  else {
    binary.result <- mclapply(datasets, function (ds) BR.run(ds, method[substitute(ds)[[3]]], mtestdata), mc.cores=min(cores, length(datasets)))
  }
  binary.result <- as.matrix(apply(sapply(binary.result, unlist), 2, as.integer))
  colnames(binary.result) <- rownames(mtraindata$labels)
  
  BR.evaluate(mtestdata, binary.result)
}

BR.evaluate <- function(mtestdata, predictions) {
  mlresult <- mldr_evaluate(mtestdata, predictions)
  attr(mlresult, "predictions") <- predictions
  
  mlresult
}

BR.run <- function (dataset, method, mtest) {
  #Test if method is valid
  fname <- paste("BR.execute", method, sep="") 
  result <- eval(call(fname, dataset, mtest))
  result
}

BR.executeSVM <- function (dataset, mtest) {
  model <- svm(as.matrix(dataset[,-ncol(dataset)]), dataset[,ncol(dataset)])
  result <- predict(model, mtest$dataset[mtest$attributesIndexes])
  
  result
}

BR.executeNB <- function (dataset, mtest) {
  model <- naiveBayes(dataset[,-ncol(dataset)], dataset[,ncol(dataset)])
  result <- predict(model, mtest$dataset[mtest$attributesIndexes])
  
  result
}

BR.executeRF <- function (dataset, mtest) {
  model <- randomForest(dataset[,-ncol(dataset)], dataset[,ncol(dataset)])
  result <- predict(model, mtest$dataset[mtest$attributesIndexes])
  
  result
}
