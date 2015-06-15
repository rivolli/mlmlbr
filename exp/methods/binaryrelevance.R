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
    binary.result <- lapply(1:length(datasets), function (i) BR.run(datasets[[i]], method[i], mtestdata))
  }
  else {
    binary.result <- mclapply(1:length(datasets), function (i) BR.run(datasets[[i]], method[i], mtestdata), mc.cores=min(cores, length(datasets)))
  }
  binary.result <- as.matrix(apply(sapply(binary.result, unlist), 2, as.integer))
  colnames(binary.result) <- rownames(mtraindata$labels)
  BR.evaluate(mtestdata, binary.result)
}

BR.evaluate <- function(mtestdata, predictions) {
  if (mtestdata$measures$num.labels != ncol(predictions)) {
    classes <- colnames(predictions)
    npred <- matrix(ncol=mtestdata$measures$num.labels, nrow=mtestdata$measures$num.instances)
    colnames(npred) <- rownames(mtestdata$labels)
    newcol <- rep(0, mtestdata$measures$num.instances)
    insert_in <- 1
    for (cls in rownames(mtestdata$labels)) {
      if (cls %in% classes) {
        npred[,insert_in] <- predictions[,cls]
      }
      else {
        npred[,insert_in] <- newcol
      }
      insert_in <- insert_in + 1
    }
    predictions <- npred
  }
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

BR.executeDT <- function (dataset, mtest) {
  classname <- names(dataset)[ncol(dataset)]
  formula <- as.formula(paste(classname, " ~ .", sep=""))
  model <- RWeka::J48(formula, dataset) #Using RWeka:: prefix because whithout it, Running with mclapply doesnt work
  result <- predict(model, mtest$dataset[mtest$attributesIndexes])
  
  result
}

BR.executeKNN <- function (dataset, k, mtest) {
  result <- knn(dataset[,-ncol(dataset)], mtest$dataset[mtest$attributesIndexes], dataset[,ncol(dataset)], k)
  
  result
}

BR.executeKNN_1 <- function (dataset, mtest) {
  BR.executeKNN(dataset, 1, mtest)
}

BR.executeKNN_3 <- function (dataset, mtest) {
  BR.executeKNN(dataset, 3, mtest)
}

BR.executeKNN_5 <- function (dataset, mtest) {
  BR.executeKNN(dataset, 5, mtest)
}

BR.executeKNN_7 <- function (dataset, mtest) {
  BR.executeKNN(dataset, 7, mtest)
}