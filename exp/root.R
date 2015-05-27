# R Code
# 14/04/2015
# A. Rivolli, A. C.P.L.F. Carvalho, 2015
# Root function call for each dataset
root <- function(file) {
  path <- get_filenames(file)

  if (!file.exists(path$resultfile) && path$datasetname == "medical") {
    cat('** Reading: ', path$datasetname, now(), '\n')
    traindata <- mldr(path$trainfile, auto_extension=FALSE, xml_file=path$xmlfile)
    if (is_sparce_data(traindata)) {
      traindata <- fill_sparce_mldrdata(traindata)
    }

    #Break in L datasets for Binary Relevance
    datasets <- lapply(mldr_transform(traindata), convertClassColumn)

    #Extract general features
    cat("  - Extract features for meta learning", now(), '\n')
    featurefile <- path$get_tempfile('features', '.RData')
    if (!file.exists(featurefile)) {
      features <- as.data.frame(do.call("rbind", mclapply(datasets, characterization, path, mc.cores=min(CORES, length(datasets)))))
      #features <- as.data.frame(do.call("rbind", lapply(datasets, characterization, path)))
      save(features, file=featurefile)
    }
    else {
      load(featurefile)
    }

    cat("  - Running for metabase generation",now(), '\n')
    #Runing K-Fold
    set.seed(traindata$measures$num.instances);
    kfoldmatrix <- get_kfoldsIndexes(traindata, 10);
    results <- mclapply(datasets, runningClassifiers, kfoldmatrix, path, mc.cores=min(CORES, length(datasets)))
    #results <- lapply(datasets, runningClassifiers, kfoldmatrix, path)
    save(results, file=path$get_rdatafile('details'))

    methods <- unlist(lapply(results, function (kpart) kpart$auc))
    accuracy <- unlist(lapply(results, function (kpart) kpart$accuracy))
    write.csv(cbind(features, methods, accuracy), file=path$resultfile, row.names=FALSE)
    rm(path, traindata, results, kfoldmatrix, features)
  }
}

#Runing classifiers
runningClassifiers <- function (binarybase, kfoldmatrix, path) {
  data_preprocessed <- normalize(replace_nominal_att(binarybase))
  classname <- colnames(data_preprocessed)[ncol(data_preprocessed)]
  resultfile <- path$get_tempfile(paste(classname, '-results', sep=''), '.RData')
  if (file.exists(resultfile)) {
      load(resultfile)
      return(resultdata)
  }

  totals <- list(BASELINE={}, KNN_1={}, KNN_3={}, KNN_5={}, KNN_7={}, SVM={}, NB={}, DT={}, RF={}) #, LM={}, LDA={}
  for (i in 1:nrow(kfoldmatrix)) {
    cat("  - K-Fold", i, " - ", classname, now(), '\n')
    trainIdx <- as.vector(kfoldmatrix[(i * -1),])
    trainIdx <- trainIdx[!is.na(trainIdx)]
    testIdx <- kfoldmatrix[i,!is.na(kfoldmatrix[i,])]

    trainData <- data_preprocessed[trainIdx,]
    testData <- data_preprocessed[testIdx,]
    labelIdx <- ncol(trainData)
    testData[,labelIdx] <- factor(testData[,labelIdx], levels=c("0","1"))

    if (sum(table(trainData[,labelIdx]) > 0) < 2) {
      cat("_____ Skipping only a single class ______\n")
      next
    }

    cat("  - BASELINE", i, " - ", classname, "\n")
    baseline.result <- rep(as.numeric(names(which.max(table(trainData[,labelIdx])))), nrow(testData))
    baseline.measures <- binary.evaluate.complete(baseline.result, testData[,labelIdx], baseline.result)
    totals$BASELINE <- rbind(totals$BASELINE, baseline.measures)

    #Running KNN 1, 3, 5, 7
    for (k in seq(1, 7, 2)) {
       methodname <- paste('KNN', k, sep='_')
       cat("      *" , classname,   " - ", methodname, "(", i, ")\n")
       knn.result <- knn(trainData[,-labelIdx], testData[,-labelIdx], trainData[,labelIdx], k, prob=T)
       knn.probs <- ifelse(knn.result == 0, 1-attr(knn.result, "prob"), attr(knn.result, "prob"))

       knn.measures <- binary.evaluate.complete(knn.result, testData[,labelIdx], knn.probs)
       totals[[methodname]] <- rbind(totals[[methodname]], knn.measures)
    }

    #Running SVM
    cat("      *" , classname, "- SVM (", i, ")\n")
    svmData <- as.matrix(trainData[,-labelIdx])
    rownames(svmData) <- NULL
    if (sum(table(trainData[,labelIdx]) > 1) == 2) { # Ha pelo menos 2 exemplos de cada classe, caso contrario o ksvm gera um erro
      svm.model <- ksvm(svmData, trainData[,labelIdx], prob.model=T)
      svm.probs <- predict(svm.model, testData[,-labelIdx], "probabilities")[,2]
      svm.result <- as.numeric(svm.probs>0.5)
    }
    else if (sum(table(trainData[,labelIdx]) > 0) == 1) {
      #Invalid result in ksvm: Nos proximos nao utilizar o ksvm
      svm.model <- svm(svmData, trainData[,labelIdx], probability=T)
      svm.result <- predict(svm.model, testData[,-labelIdx], probability = T)
      svm.probs <- attr(svm.result, "probabilities")[,2]
    }
    else {
      #Invalid result to ksvm and svm so complete result with 0 values
      cat("________ Invalid traindata to SVM classifier ________\n")
      svm.result <- rep(2, length(testIdx)) #All values are wrong
      svm.probs <- rep(0, length(testIdx)) #All probabilites are 0
    }
    svm.measures <- binary.evaluate.complete(svm.result, testData[,labelIdx], svm.probs)
    totals$SVM <- rbind(totals$SVM, svm.measures)

    #Running NaiveBayes
    cat("      *" , classname, "- Naive Bayes (", i, ")\n")
    nb.model <- naiveBayes(trainData[,-labelIdx], trainData[,labelIdx], type="raw")
    nb.probs <- predict(nb.model, testData[,-labelIdx], "raw")[,2]
    nb.result <- as.numeric(nb.probs>0.5)

    nb.measures <- binary.evaluate.complete(nb.result, testData[,labelIdx], nb.probs)

    totals$NB <- rbind(totals$NB, nb.measures)

    #Running Linear Discriminant Analysis
#     lda.model <- lda(trainData[,-labelIdx], trainData[,labelIdx])
#     lda.pred <- predict(lda.model, testData[,-labelIdx])
#     lda.result <- lda.pred$class
#     lda.probs <- lda.pred$posterior[,2]
#
#     lda.measures <- binary.evaluate.complete(lda.result, testData[,labelIdx], lda.probs)
#     totals$LDA <- rbind(totals$LDA, lda.measures)

    #Running Decision Tree
    cat("      *" , classname, "- Decision Tree (", i, ")\n")
    formula <- as.formula(paste(classname, " ~ .", sep=""))
    dt.model <- RWeka::J48(formula, trainData) #Using RWeka:: prefix because whithout it, Running with mclapply doesnt work
    dt.probs <- predict(dt.model, testData[,-labelIdx], "probability")[,2]
    dt.result <- as.numeric(dt.probs>0.5)

    dt.measures <- binary.evaluate.complete(dt.result, testData[,labelIdx], dt.probs)
    totals$DT <- rbind(totals$DT, dt.measures)

    #Running Random Forest
    cat("      *" , classname, "- Random Forest (", i, ")\n")
    if (sum(table(trainData[,labelIdx]) > 0) >= 1) {
      rf.model <- randomForest(trainData[,-labelIdx], trainData[,labelIdx])
      rf.probs <- predict(rf.model, testData[,-labelIdx], "prob")[,2]
      rf.result <- as.numeric(rf.probs>0.5)
    }
    else {
      #Invalid result to randomForest so complete result with 0 values
      cat("________ Invalid traindata to randomForest classifier ________\n")
      rf.result <- rep(2, length(testIdx)) #All values are wrong
      rf.probs <- rep(0, length(testIdx)) #All probabilites are 0
    }
    rf.measures <- binary.evaluate.complete(rf.result, testData[,labelIdx], rf.probs)
    totals$RF <- rbind(totals$RF, rf.measures)
  }

  if (is.list(totals$BASELINE)) {
    #Para alguns datasets nao ha exemplos validos para uma determinada classe, neste caso nada foi computado entao o fluxo cai no else
    summary <- lapply(totals, function (method) apply(method, 2, get_min_max_mean_sd, ''))
    methodauc <- names(which.max(unlist(lapply(summary, function (m) m["_mean","AUC"]))))
    methodacc <- names(which.max(unlist(lapply(summary, function (m) m["_mean","BalancedAccuracy"]))))
  }
  else {
    summary <- list()
    methodauc <- "invalid"
    methodacc <- "invalid"
  }
  resultdata <- list(auc=methodauc, accuracy=methodacc, data=totals, summary=summary)
  save(resultdata, file=resultfile)

  resultdata
}

#Convert the class column as factor
convertClassColumn <- function (df) {
  df[,ncol(df)] <- as.factor(df[,ncol(df)])
  df
}

#Return the file-names
#@param file Is the path to the train dataset in arff format
get_filenames <- function (file) {
  path <- NULL;
  parts <- unlist(strsplit(file, '/'));
  path$basepath <- paste(paste(parts[-length(parts)], collapse="/"), '/', sep='');
  path$datasetname <- parts[length(parts)-1];
  path$trainfile <- file;
  path$xmlfile <- gsub("-train.arff",".xml", file);
  path$testfile <- gsub("-train.arff","-test.arff", file);
  path$resultfile <- paste(OUTPUT, path$datasetname, '.csv', sep='');
  path$datasetinfo <- paste(OUTPUT, path$datasetname, '-info.csv', sep='');
  path$get_rdatafile <- function (complement) { return(paste(OUTPUT, 'RData/', path$datasetname, '-', complement, '.RData', sep='')) };
  path$get_tempfile <- function (method, extension='.csv') { return(paste(OUTPUT, 'extra/', path$datasetname, '-', method, extension, sep='')); };
  return (path);
}
