#Generate metabase with data results
# 09/06/2015
# A. Rivolli, A. C.P.L.F. Carvalho, 2015
setup_metabase <- function() {
  files <- list.files("exp/", recursive=TRUE, full.name=TRUE);
  for(i in files) source(i);
}

run_metabase <- function () {
   setup_metabase();
  
   results <- load_datasets()
   metainfo <- generate_metabase(results)
   if (CORES > 1) {
     mtlres <- mclapply(names(results), runMTLclassify, metainfo, mc.cores=min(CORES, length(results)))
   }
   else {
     mtlres <- lapply(names(results), runMTLclassify, metainfo)
   }
   names(mtlres) <- names(results)
   tbls <- do.call(rbind, mtlres)
   cat("Accuracy: ", 1 - sum(apply(tbls, 1, function (row) row["error"] * row["tests"])) / sum(tbls[,"tests"]), "\n")
   write.csv(tbls, "metabase.resuls.csv")
  
   datagraphics <- generate_datagraphics(results)
   #show_plot_classifiers(datagraphics$methodsauc, "AUC Results")
   show_plot_classifiers(datagraphics$methodsacc, "F1 Measure Results")
     
   infores <- load_datasets_info()
   infographics <- generate_infographics(infores)
   show_plot_infocomparation(infographics)

#    for (measure in c("Accuracy", "AUC", "Recall", "Precision", "AveragePrecision", "FMeasure", "HammingLoss", "SubsetAccuracy", "MacroFMeasure", "MicroFMeasure")) {
#      comparative <- matrix(unlist(lapply(infores, function (metrics) metrics[c("SVM", "PRED"),measure])), ncol=2, byrow=T)
#      cat(measure, "SVM:", mean(comparative[,1]), "MTL:", mean(comparative[,2]), wilcoxon(comparative[,1], comparative[,2], .95), "\n")
#    }

  TRUE
}

load_datasets <- function () {
  datasets <- list()
  for(file in FILES) {
    path <- get_filenames(file)
    if (file.exists(path$resultfile)) {
      datasets[[path$datasetname]] <- read.csv.file(path$resultfile)
      
      load(path$get_tempfile('TOP3classifiers', '.RData')) #classifiers
      map <- change_special_chars(rownames(datasets[[path$datasetname]]))
      names(map) <- rownames(datasets[[path$datasetname]])#, "KNN_3"
      real <- factor(classifiers, levels=c("SVM", "KNN_3", "RF")) # "DT", "KNN_1", "KNN_3", "KNN_5", "KNN_7"))
      datasets[[path$datasetname]][,"real"] <- real[map]
      rownames(datasets[[path$datasetname]]) <- paste(path$datasetname, rownames(datasets[[path$datasetname]]), sep='_')
    }
  }

  datasets
}

generate_metabase <- function (results) {
  fim <- c(nrow(results[[1]]))
  ini <- c(1)
  metabase <- results[[1]]
  for (i in 2:length(results)) {
    metabase <- rbind(metabase, results[[i]])
    ini[i] <- fim[i-1] + ini[i-1]
    fim[i] <- nrow(results[[i]])
  }
  fim <- cumsum(fim)
  names(ini) <- names(results)
  names(fim) <- names(results)
  
  metabase <- metabase[!colnames(metabase) %in% 
      c(
           "Cls", "Num", "Nom", "Lda", "Nb", "Nn"
         , "auc", "accuracy", "topauc", "topaccuracy"
         , "ClMean", "Fnd", "SymMin", "NAtrEnt"
         , "Nlbst", "NumRate", "NomRate", "SymMin",	"SymMax",	"SymMean",	"SymSd",	"SymSum"
         #, "Fnd", "AtrEnt"
         #, "Spl", "Dim"
         #, "Atr"
         , "NSlbst", "Mfreq", "LCard", "LDen", "Mir", "Scl"
      )]
  colnames(metabase)[ncol(metabase)] <- "class"
  metabase[,ncol(metabase)] <- as.factor(metabase[,ncol(metabase)])
  
  lvs <- levels(metabase[,ncol(metabase)])
  lclassifiers <- list()
  for(file in FILES) {
    path <- get_filenames(file)
    load(path$get_tempfile('TOP3classifiers', '.RData')) #classifiers
    lclassifiers[[path$datasetname]] <- factor(classifiers, levels=lvs)
  }
  
  list(metabase=metabase, realbest=lclassifiers, ia=ini, ib=fim)
}

load_datasets_info <- function () {
  datasets <- list()
  for(file in FILES) {
    path <- get_filenames(file)
    if (file.exists(path$datasetinfo))
      datasets[[path$datasetname]] <- read.csv.file(path$datasetinfo)
  }
  
  datasets
}

generate_infographics <- function (results) {
  ret <- list()
  methods <- c("SVM", "TOP3") # , "TOP3"
  for (metric in c("Accuracy", "SubsetAccuracy", "HammingLoss", "FMeasure"
                   #, "AUC", "MacroFMeasure", "MicroFMeasure","OneError"
  )) {
    data <- matrix(
      rep(0, length(names(results)) * length(methods)), ncol=length(methods),
      dimnames=list(names(results), methods)
    )
    for (dsname in names(results)) {
      data[dsname,] <- results[[dsname]][methods, metric]
      #data[dsname,"MY"] <- max(results[[dsname]][c("AUC", "ACC"), metric]) #select the better result (auc or acc)
      #if (metric == "HammingLoss")
      #  data[dsname,"MY"] <- min(results[[dsname]][c("AUC", "ACC"), metric]) #select the better result (auc or acc)
    }
    ret[[metric]] <- data
  }
  ret
}

generate_datagraphics <- function (results) {
  methods <- c("SVM", "KNN_3", "RF") #"DT", "KNN_1", "KNN_3", "KNN_5", "KNN_7")
  dfauc <- matrix(
    rep(0, (length(names(results))+1) * length(methods)), ncol=length(methods),
    dimnames=list(c(names(results), c("All")), methods)
  )
  
  dfacc <- dfauc
  for (dsname in names(results)) {
    #classifierauc <- table(results[[dsname]][,"topauc"])
    classifierauc <- table(results[[dsname]][,"real"])
    for (methods in names(classifierauc)) {
      #if (methods %in% c("KNN_1", "KNN_3", "KNN_5", "KNN_7")) {
      #  dfauc[dsname, "KNN"] <- dfauc[dsname, "KNN"] + classifierauc[methods]
      #}
      #else {
        dfauc[dsname, methods] <- classifierauc[methods]  
      #}
      
    }

    classifieracc <- table(results[[dsname]][,"real"]) #topaccuracy
    for (methods in names(classifieracc)) {
      #if (methods %in% c("KNN_1", "KNN_3", "KNN_5", "KNN_7")) {
      #  dfacc[dsname, "KNN"] <- dfacc[dsname, "KNN"] + classifieracc[methods]
      #}
      #else {
        dfacc[dsname, methods] <- classifieracc[methods]
      #}
    }
  }
  dfauc["All",] <- apply(dfauc, 2, sum)
  dfacc["All",] <- apply(dfacc, 2, sum)
  
  list(methodsauc=dfauc, methodsacc=dfacc)
}

show_plot_classifiers <- function (data, title) {
  df1 <- melt(prop.table(data, 1))
  df1 <- cbind(df1, total = melt(data)[, 3])
  df1$total <- ifelse(df1$total == 0, "", df1$total)
  
  g <- ggplot(data=df1, aes(x=1, y=value, fill=Var2, group=Var2))
  g <- g + geom_bar(stat="identity") + coord_polar(theta="y") 
  
  g <- g + facet_wrap(~Var1)
  g <- g + ggtitle(title) + ylab("") + xlab("") + labs(fill="Classifiers")
  plot(g)

  df2 <- melt(data)
  df2 <- cbind(df2, total = melt(data)[, 3])
  df2$total <- ifelse(df2$total == 0, "", df2$total)
  g <- ggplot(data=df2, aes(x=Var2, y=value, fill=Var2, group=Var2, label=value))
  g <- g + geom_bar(position="dodge", stat="identity") + geom_text()
  g <- g + facet_wrap(~Var1, scales="free", ncol=3)
  g <- g + ggtitle(title) + ylab("") + xlab("") + labs(fill="Classifiers") +
    theme(legend.position="bottom")
  plot(g)
}

show_plot_infocomparation <- function (datagraphics) {
  for(metric in names(datagraphics)) {
    colnames(datagraphics[[metric]]) <- c("SVM", "MTL")
    df <- melt(datagraphics[[metric]])
    colnames(df) <- c("Dataset", "Method", "Value")
    g <- ggplot(data=df, aes(x=Dataset, y=Value, group=Method, colour=Method))
    g <- g + geom_line(aes(linetype=Method, colour=Method), size=1)
    g <- g + geom_point(size=2)
    #g <- g + geom_text(aes(label=value),hjust=0, vjust=0)
    #g <- g + scale_fill_hue(name="Classifiers")
    #g <- g + scale_linetype_discrete(name="Classifiers")
    g <- g + ggtitle(paste(metric, "Comparative")) + ylab(metric) + xlab("Datasets")
    g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    plot(g)
  }
}