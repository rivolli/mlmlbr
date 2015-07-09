
setup = function() {
  files = list.files("exp/", recursive=TRUE, full.name=TRUE);
  for(i in files) source(i);
}

do.results <- function () {
  setup()
  methods <- c("SVM", "RF", "KNN_3", "RANDOM")
  folds <- c("Acc", "BalAcc", "F1")
  allmethods <- c(methods, folds)
  mlmetrics <- c("Accuracy", "SubsetAccuracy", "HammingLoss", "FMeasure", "Precision", "Recall", 
                "MacroAUC", "MacroFMeasure", "MacroPrecision", "MacroRecall",
                "MicroAUC",	"MicroFMeasure", "MicroPrecision", "MicroRecall")
  
  metabase <- read.csv.file('results/000 - metabase.csv')
  datasets <- as.character(unique(metabase[,"datasetname"]))
  
  rmetrics <- list()
  for (method in allmethods) {
    rmetrics[[method]] <- sapply(mlmetrics, function (metric) {
      values <- rep(0, length(datasets))
      names(values) <- datasets
      values
    })
  }
  
  for (ds in datasets) {
    #Acc, SVM, NB, KNN_3, Random
    filename <- paste('results/final/Acc/', ds, '-info.csv', sep='')
    results <- read.csv.file(filename)
    for (method in methods) {
      rmetrics[[method]][ds, mlmetrics] <- as.matrix(results[method, mlmetrics])
    }
    rmetrics[["Acc"]][ds, mlmetrics] <- as.matrix(results["TOP3", mlmetrics])
    
    #BalAcc
    filename <- paste('results/final/BalAcc/', ds, '-info.csv', sep='')
    results <- read.csv.file(filename)
    rmetrics[["BalAcc"]][ds, mlmetrics] <- as.matrix(results["TOP3", mlmetrics])
    
    #F1
    filename <- paste('results/final/F1/', ds, '-info.csv', sep='')
    results <- read.csv.file(filename)
    rmetrics[["F1"]][ds, mlmetrics] <- as.matrix(results["TOP3", mlmetrics])
  }
  
  #Ranking
  ranking.pos <- list()
  ranking.val <- list()
  ranking.stats <- list()
  for (metric in mlmetrics) {
    ranking.pos[[metric]] <- matrix(nrow=length(datasets), ncol=length(allmethods), dimnames=list(datasets, allmethods))
    ranking.val[[metric]] <- matrix(nrow=length(datasets), ncol=length(allmethods), dimnames=list(datasets, allmethods))
    ranking.stats[[metric]] <- matrix(nrow=length(folds), ncol=length(methods), dimnames=list(folds, methods))
    seq <- 7:1
    if (metric == "HammingLoss") seq <- 1:7
    for (ds in datasets) {
      for (method in allmethods) {
        ranking.val[[metric]][ds,method] <- rmetrics[[method]][ds,metric]
      }
      ranking.pos[[metric]][ds,names(sort(ranking.val[[metric]][ds,]))] <- seq
    }
    
    for (f in folds) {
      for (method in methods) {
        ranking.stats[[metric]][f, method] <- wilcoxon(ranking.val[[metric]][,f], ranking.val[[metric]][,method], .95)        
      }
    }
  }
  browser()
  comp.ranking <- do.call(rbind, lapply(ranking.val, function (table){
    round(apply(table, 2, mean), 1)
  }))
  
  comp.meansd <- do.call(rbind, lapply(rmetrics, function (table) {
    rbind(round(apply(table, 2, mean), 3), round(apply(table, 2, sd), 2))
  }))
  rownames(comp.meansd) <- paste(rep(names(rmetrics), rep(2, length(rmetrics))), c("mean", "sd"), sep='_')
  View(cbind(t(comp.meansd), comp.ranking))
  
  browser()
}