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
     results <- mclapply(names(results), runMTLclassify, metainfo, mc.cores=min(CORES, length(results)))
   }
   else {
     results <- lapply(names(results), runMTLclassify, metainfo)
   }
  
#   datagraphics <- generate_datagraphics(results)
#   show_plot_classifiers(datagraphics$methodsauc, "AUC Results")
#   show_plot_classifiers(datagraphics$methodsacc, "Accuracy Results")
#     
#   infores <- load_datasets_info()
#   infographics <- generate_infographics(infores)
#   show_plot_infocomparation(infographics)

  TRUE
}

load_datasets <- function () {
  datasets <- list()
  for(file in FILES) {
    path <- get_filenames(file)
    if (file.exists(path$resultfile)) {
      datasets[[path$datasetname]] <- read.csv.file(path$resultfile)
      load(path$get_tempfile('TOP3classifiers', '.RData'))
      datasets[[path$datasetname]] <- cbind(datasets[[path$datasetname]], classifiers)
      colnames(datasets[[path$datasetname]])[ncol(datasets[[path$datasetname]])] <- "realacc"
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
  
  metabase <- metabase[!colnames(metabase) %in% c("Cls", "Nom", "Lda", "Nb", "Nn", "auc", "accuracy", "topauc")]
  colnames(metabase)[ncol(metabase)] <- "class"
  metabase[,ncol(metabase)] <- as.factor(metabase[,ncol(metabase)])
 
  list(metabase=metabase, ia=ini, ib=fim)
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
  for (metric in c("Accuracy", "SubsetAccuracy", "FMeasure", "HammingLoss")) {
    data <- matrix(
      rep(0, length(names(results)) * 4), ncol=4,
      dimnames=list(names(results), c("SVM", "MY", "TOP3", "ALL"))
    )
    for (dsname in names(results)) {
      data[dsname,] <- results[[dsname]][c("SVM", "AUC", "TOP3", "ALL"), metric]
      data[dsname,"MY"] <- max(results[[dsname]][c("AUC", "ACC"), metric]) #select the better result (auc or acc)
      if (metric == "HammingLoss")
        data[dsname,"MY"] <- min(results[[dsname]][c("AUC", "ACC"), metric]) #select the better result (auc or acc)
    }
    ret[[metric]] <- data
  }
  ret
}

generate_datagraphics <- function (results) {
  dfauc <- matrix(
    rep(0, (length(names(results))+1) * 3), ncol=3,
    dimnames=list(c(names(results), c("All")), c("SVM", "NB", "RF"))
  )
  
  dfacc <- dfauc
  for (dsname in names(results)) {
    classifierauc <- table(results[[dsname]][,"topauc"])
    for (methods in names(classifierauc)) {
      #if (methods %in% c("KNN_1", "KNN_3", "KNN_5", "KNN_7")) {
      #  dfauc[dsname, "KNN"] <- dfauc[dsname, "KNN"] + classifierauc[methods]
      #}
      #else {
        dfauc[dsname, methods] <- classifierauc[methods]  
      #}
      
    }

    classifieracc <- table(results[[dsname]][,"topaccuracy"])
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
  g <- g + facet_wrap(~Var1, scales="free")
  g <- g + ggtitle(title) + ylab("") + xlab("") + labs(fill="Classifiers")
  plot(g)
}

show_plot_infocomparation <- function (datagraphics) {
  for(metric in names(datagraphics)) {
    df <- melt(datagraphics[[metric]])
    g <- ggplot(data=df, aes(x=Var1, y=value, group=Var2))
    g <- g + geom_line(aes(colour=Var2, linetype=Var2), size=1)
    g <- g + geom_point()
    #g <- g + scale_fill_hue(name="Classifiers")
    #g <- g + scale_linetype_discrete(name="Classifiers")
    g <- g + ggtitle(paste(metric, "Comparative")) + ylab(metric) + xlab("Datasets")
    g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    plot(g)
  }
}