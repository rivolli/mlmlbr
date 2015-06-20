# R Code
# 14/04/2015
# A. Rivolli, A. C.P.L.F. Carvalho, 2015
# Root function call for each dataset
root <- function(file) {
  path <- get_filenames(file)

  if (!file.exists(path$resultfile)) { # && path$datasetname == "flags") {
    cat('** Reading: ', path$datasetname, now(), '\n')
    traindata <- mldr(path$trainfile, auto_extension=FALSE, xml_file=path$xmlfile)
    if (is_sparce_data(traindata)) {
      traindata <- fill_sparce_mldrdata(traindata)
    }

    #remove unique columns and labels with lower than 10 examples of each value
    traindata <- remove_unique_attributes(traindata, 1) #this row may throw an error when all labels have lower than 10 values for each label
    
    #Break in L datasets for Binary Relevance
    datasets <- lapply(mldr_transform(traindata), convertClassColumn)
 
    #Extract general features
    cat("  - Extract features for meta learning", now(), '\n')
    featurefile <- path$get_tempfile('features', '.RData')
#     if (!file.exists(featurefile)) {
       if (CORES > 1) {
         features <- as.data.frame(do.call("rbind", mclapply(datasets, characterization, path, mc.cores=min(CORES, length(datasets))))) 
       }
       else {
         features <- as.data.frame(do.call("rbind", lapply(datasets, characterization, path)))
       }
#       save(features, file=featurefile)
#     }
#     else {
#       load(featurefile)
#     }
    return();
    rownames(features) <- rownames(traindata$labels)

    cat("  - Running for metabase generation",now(), '\n')
    resultfile <- path$get_rdatafile('details')
    if (!file.exists(resultfile)) {
      cat("Running 10-Fold Cross Validation", now(), "\n")

      #Runing K-Fold
      set.seed(traindata$measures$num.instances)
      kfoldmatrix <- get_kfoldsIndexes(traindata, 10)
      
      if (CORES > 1) {
        results <- mclapply(datasets, runningClassifiers, kfoldmatrix, path, mc.cores=min(CORES, length(datasets)))
      }
      else {
        results <- lapply(datasets, runningClassifiers, kfoldmatrix, path)
      }
      
      save(results, file=resultfile)
    }
    else {
      cat("Loading result from file", now(), "\n")
      load(resultfile)
    }

    #Removing KNN, Baseline and DT from results to learn a metaclassifier more simple
    for (i in 1:length(results)) {
      cat("- Recalculating class: ", i, now(), "\n")
      results[[i]]$summary$BASELINE <- NULL
      results[[i]]$summary$KNN_1 <- NULL
      results[[i]]$summary$KNN_3 <- NULL
      results[[i]]$summary$KNN_5 <- NULL
      results[[i]]$summary$KNN_7 <- NULL
      results[[i]]$summary$DT <- NULL
      
      results[[i]]$topauc <- names(which.max(unlist(lapply(results[[i]]$summary, function (m) m["_mean","AUC"]))))
      results[[i]]$topaccuracy <- names(which.max(unlist(lapply(results[[i]]$summary, function (m) m["_mean","BalancedAccuracy"]))))  
    }

    #Multilabel data metafeatures
    mlfeatures <- data.frame(
      Nlbst=rep(traindata$measures$num.labelsets, nrow(features)),
      NSlbst=rep(traindata$measures$num.single.labelsets, nrow(features)),
      Mfreq=rep(traindata$measures$max.frequency, nrow(features)),
      LCard=rep(traindata$measures$cardinality, nrow(features)),
      LDen=rep(traindata$measures$density, nrow(features)),
      Mir=rep(traindata$measures$meanIR, nrow(features)),
      Scl=rep(traindata$measures$scumble, nrow(features)),
      Lfq=rep(0, nrow(features)),
      IRLbl=rep(0, nrow(features)),
      LScl=rep(0, nrow(features)),
      row.names = rownames(features)
    )
    
    for (cls in rownames(mlfeatures)) {
      mlfeatures[cls, c("Lfq", "IRLbl", "LScl")] <- traindata$labels[cls, c("freq", "IRLbl", "SCUMBLE")]
    }
    
    auc <- unlist(lapply(results, function (kpart) kpart$auc))
    accuracy <- unlist(lapply(results, function (kpart) kpart$accuracy))
    topauc <- unlist(lapply(results, function (kpart) kpart$topauc))
    topaccuracy <- unlist(lapply(results, function (kpart) kpart$topaccuracy))
    
    write.csv(cbind(features, mlfeatures, auc, accuracy, topauc, topaccuracy), file=path$resultfile)
    rm(traindata, results, features)
  }
  
  #Multilabel results
  if (!file.exists(path$datasetinfo)){ # && path$datasetname != "medical") { #
    cat('** Reading: ', path$datasetname, now(), '\n')
    traindata <- mldr(path$trainfile, auto_extension=FALSE, xml_file=path$xmlfile)
    testdata <- mldr(path$testfile, auto_extension=FALSE, xml_file=path$xmlfile)
    ds <- mldr_preprocess(traindata, testdata)
    traindata <- ds[[1]]
    testdata <- ds[[2]]
    
    contentresult <- runningExperimentsEvaluation(traindata, testdata, path)
    write.csv(contentresult, file=path$datasetinfo)
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
    #cat("  - K-Fold", i, " - ", classname, now(), '\n')
    trainIdx <- as.vector(kfoldmatrix[(i * -1),])
    trainIdx <- trainIdx[!is.na(trainIdx)]
    testIdx <- kfoldmatrix[i,!is.na(kfoldmatrix[i,])]

    trainData <- data_preprocessed[trainIdx,]
    testData <- data_preprocessed[testIdx,]
    labelIdx <- ncol(trainData)
    testData[,labelIdx] <- factor(testData[,labelIdx], levels=c("0","1"))

    if (sum(table(trainData[,labelIdx]) > 0) < 2) {
      cat("_____ Skipping only a single class ______\n")
      rm(trainIdx, testIdx, trainData, testData, labelIdx)
      next
    }

    cat("      *", now(), classname, "- BASELINE (", i, ")\n")
    totals$BASELINE <- rbind(totals$BASELINE, get_baseline_measures(trainData, testData, labelIdx))
    
    #Running KNN 1, 3, 5, 7
    for (k in seq(1, 7, 2)) {
       methodname <- paste('KNN', k, sep='_')
       cat("      *", now(), classname, "-", methodname, "(", i, ")\n")
       totals[[methodname]] <- rbind(totals[[methodname]], get_knn_measures(trainData, testData, labelIdx, k))       
    }

    #Running SVM
    cat("      *", now(), classname, "- SVM (", i, ")\n")
    totals$SVM <- rbind(totals$SVM, get_svm_measures(trainData, testData, labelIdx, testIdx))

    #Running NaiveBayes
    cat("      *", now(), classname, "- Naive Bayes (", i, ")\n")
    totals$NB <- rbind(totals$NB, get_naivebayes_measures(trainData, testData, labelIdx))

    #Running Decision Tree
    cat("      *" , now(), classname, "- Decision Tree (", i, ")\n")
    totals$DT <- rbind(totals$DT, get_decisiontree_measures(trainData, testData, labelIdx, classname))

    #Running Random Forest
    cat("      *", now(), classname, "- Random Forest (", i, ")\n")
    totals$RF <- rbind(totals$RF, get_randomforest_measures(trainData, testData, labelIdx, testIdx))

    rm(trainIdx, testIdx, trainData, testData, labelIdx)
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
  gc()

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
