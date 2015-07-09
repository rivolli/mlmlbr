
setup = function() {
  files = list.files("exp/", recursive=TRUE, full.name=TRUE);
  for(i in files) source(i);
}

do.results <- function () {
  setup()
  methods <- c("SVM", "RF", "KNN_3")
  allmethods <- c(methods, c("Acc", "BalAcc", "F1"))
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
  browser()
}