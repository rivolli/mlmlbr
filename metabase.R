#Generate metabase with data results
# 09/06/2015
# A. Rivolli, A. C.P.L.F. Carvalho, 2015
setup_metabase <- function() {
  files <- list.files("exp/", recursive=TRUE, full.name=TRUE);
  for(i in files) source(i);
}

run_metabase <- function () {
  setup_metabase();
  
  results <- load_datasets();
  datagraphics <- generate_datagraphics(results)
  show_plot_classifiers(datagraphics$methodsauc, "AUC Results")
  show_plot_classifiers(datagraphics$methodsacc, "Accuracy Results")
  
  TRUE
}

load_datasets <- function () {
  datasets <- list()
  for(file in FILES) {
    path <- get_filenames(file)
    datasets[[path$datasetname]] <- read.csv.file(path$resultfile)
  }
  
  datasets
}

generate_datagraphics <- function (results) {
  dfauc <- matrix(
    rep(0, (length(names(results))+1) * 3), ncol=3,
    dimnames=list(c(names(results), c("All")), c("SVM", "NB", "RF"))
  )
  
  dfacc <- dfauc
  for (dsname in names(results)) {
    classifierauc <- table(results[[dsname]][,"auc"])
    for (methods in names(classifierauc)) {
      #if (methods %in% c("KNN_1", "KNN_3", "KNN_5", "KNN_7")) {
      #  dfauc[dsname, "KNN"] <- dfauc[dsname, "KNN"] + classifierauc[methods]
      #}
      #else {
        dfauc[dsname, methods] <- classifierauc[methods]  
      #}
      
    }
    classifieracc <- table(results[[dsname]][,"accuracy"])
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