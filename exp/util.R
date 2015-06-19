now <- function () {
  return(as.character(Sys.time()));
}

#Read a CSV file with rowname (The first column is the rowname)
read.csv.file <- function (file) {
  table <- read.csv(file)
  rownames(table) <- table[,1]
  table[,-1]
}

#If data has NA it is sparce
is_sparce_data <- function (mdata) {
  sum(apply(mdata$dataset, 2, function (col) sum(!complete.cases(col)))) > 0
}

#Fill NA with 0 or "" values for
fill_sparce_mldrdata <- function (mdata) {
  dataset <- data.frame(row.names=1:mdata$measures$num.instances)
  dataset <- cbind(dataset, lapply(mdata$dataset[,1:mdata$measures$num.attributes], function (col){
    if (sum(!complete.cases(col)) > 0) {
      #Has NA value
      if (is.numeric(col)) {
        #Numeric value - fill with 0
        col[is.na(col)] <- 0
      }
      else if (sum(!is.na(as.numeric(col[!is.na(col)]))) > 0) {
        #Text but with numeric values - convert to numeric and filll with 0
        col[is.na(col)] <- "0"
        col <- as.numeric(col)
      }
      else {
        #Text value - fill with ""
        col[is.na(col)] <- ""
      }
    }
    col
  }))

  mldr_from_dataframe(dataset, mdata$labels$index, mdata$name)
}

#Remove unique attributes and Remove empty class with less than limitecls
remove_unique_attributes <- function (mdata, limitecls=0, tmdata=NULL) {
  dataset <- data.frame(row.names=1:mdata$measures$num.instances)
  clsIndex <- numeric(0)
  
  #attributes
  if (is.null(tmdata)) {
    for (i in mdata$attributesIndexes) {
      if (length(unique(mdata$dataset[,i])) > 1) {
        dataset <- cbind(dataset, mdata$dataset[i])
      }
    }
  }
  else {
    tdataset <- data.frame(row.names=1:tmdata$measures$num.instances)
    for (i in mdata$attributesIndexes) {
      if (length(unique(mdata$dataset[,i])) > 1 && length(unique(tmdata$dataset[,i])) > 1) {
        dataset <- cbind(dataset, mdata$dataset[i])
        tdataset <- cbind(tdataset, tmdata$dataset[i])
      }
    }
  }
  
  #labels - only from train data because testdata the classes can't be changed
  for (i in mdata$labels$index) {
    #Test if there are at least two classes and if each them are at least limitecls values
    if (length(unique(mdata$dataset[,i])) > 1 && all(table(mdata$dataset[,i]) > limitecls)) {
      dataset <- cbind(dataset, mdata$dataset[i])
      clsIndex[length(clsIndex)+1] <- ncol(dataset)
    }
  }
  
  #TODO remove classes with label 0 for all instances
  
  if (is.null(tmdata)) {
    return(mldr_from_dataframe(dataset, clsIndex, mdata$name))
  }
  else {
    tclsIndex <- (1:tmdata$measures$num.labels) + ncol(tdataset)
    tdataset <- cbind(tdataset, tmdata$dataset[tmdata$labels$index])
    return (list(
      mldr_from_dataframe(dataset, clsIndex, mdata$name),
      mldr_from_dataframe(tdataset, tclsIndex, tmdata$name)
    ))
  }
}

#Preprocess a mldr data set and return a new mldr dataset with the preprocessed dataset
#If tmdata is not null then all preprocess applied to mdata will be setted to tmdata
#Parameters:
#  @param boolean transform.sparce If dataset is sparce then the features will be filled
#  @param boolean transform.numeric All Non numeric fields will be converted
#  @param boolean normalize The data will be normalized
#  @param boolean remove.uniques The unique attributes will be removed
#  @param integer limitecls When remove.unique is true then all classes with fewer examples than the limit will be removed
#
# @returns 2 differents types: a single mdata when tmdata is null or a list(mdata, tmdata)
mldr_preprocess <- function (mdata, tmdata=NULL, transform.sparce=T, transform.numeric=T, normalize=T, remove.uniques=T, limitecls=1) {
  if (transform.sparce) {
    if (is_sparce_data(mdata)) mdata <- fill_sparce_mldrdata(mdata)
    if (!is.null(tmdata) && is_sparce_data(tmdata)) tmdata <- fill_sparce_mldrdata(tmdata)
  }
  if (transform.numeric) {
    if (is.null(tmdata)) {
      mdata <- mldr_replace_nominal_att(mdata)
    }
    else {
      ds <- mldr_replace_nominal_att(mdata, tmdata)
      mdata <- ds[[1]]
      tmdata <- ds[[2]]
    }
  }
  
  if (normalize) {
    mdata <- mldr_normalize(mdata)
    if (!is.null(tmdata)) tmdata <- mldr_normalize(tmdata)
  }
  
  if (remove.uniques) {
    if (is.null(tmdata)) {
      mdata <- remove_unique_attributes(mdata, limitecls)
    }
    else {
      ds <- remove_unique_attributes(mdata, limitecls, tmdata)
      mdata <- ds[[1]]
      tmdata <- ds[[2]]
    }
  }
  
  if (is.null(tmdata)) {
    return (mdata)
  } 
  else {
    return (list(mdata, tmdata))
  }
}

# *-*-*-* CAUTION *-*-*-*
# This works only to mldr data with class index in the lasts columns
# @returns 2 differents types: a single mdata when tmdata is null or a list(mdata, tmdata)2
mldr_replace_nominal_att <- function(mdata, tmdata=NULL) {
  result <- data.frame(row.names=1:mdata$measures$num.instances)
  if (is.null(tmdata)) {
    #Only one mdata
    for(i in mdata$attributesIndexes) {
      if (is.numeric(mdata$dataset[,i])) {
        result <- cbind(result, mdata$dataset[i])
      } else {
        result <- cbind(result, mldr_replace_nominal_column(mdata$dataset[,i], colnames(mdata$dataset[i])))
      }
    }
  }
  else {
    #Train and Test mldr data
    tresult <- data.frame(row.names=1:tmdata$measures$num.instances)
    for(i in mdata$attributesIndexes) {
      if (is.numeric(mdata$dataset[,i])) {
        #TODO test if test column is also numeric
        result <- cbind(result, mdata$dataset[i])
        tresult <- cbind(tresult, tmdata$dataset[i])
      } else {
        #TODO test if test column is also not numeric
        columns <- mldr_replace_nominal_column2(mdata$dataset[,i], tmdata$dataset[,i], colnames(mdata$dataset[i]))
        result <- cbind(result, columns[[1]])
        tresult <- cbind(tresult, columns[[2]])
      }
    }
  }
  
  classIndexes <- (1:mdata$measures$num.labels) + ncol(result)
  result <- cbind(result, mdata$dataset[mdata$labels$index])

  if (is.null(tmdata)) {
    return (mldr_from_dataframe(data.frame(result), classIndexes, mdata$name))
  }
  else {
    tresult <- cbind(tresult, tmdata$dataset[tmdata$labels$index])
    return (list(
      mldr_from_dataframe(data.frame(result), classIndexes, mdata$name),
      mldr_from_dataframe(data.frame(tresult), classIndexes, tmdata$name)
    ))
  }
  
}

mldr_replace_nominal_column <- function(column, column.name='', type=1) {
  column <- as.factor(column)
  symbols <- levels(column)
  result <- {}
  if (length(symbols) == 2 && type == 1 && 0 %in% symbols && 1 %in% symbols) {
    result <- cbind(result, as.double(column == 1))
    names <- column.name
  }
  else {
    for (i in 1:(length(symbols)-type)) {
      result <- cbind(result, as.double(column == symbols[i]))
    }
    names <- paste(column.name, symbols[1:(length(symbols)-type)], sep='_')
  }
  if (column.name != '') {
    colnames(result) <- names
  }
  
  result
}

#Merge train and test values to make same columns 
mldr_replace_nominal_column2 <- function(column, tcolumn, column.name='') {
  #TODO ignore type
  column <- as.factor(column)
  tcolumn <- as.factor(tcolumn)
  symbols <- unique(c(levels(column), levels(tcolumn))) #joint train and test levels
  result <- {}
  tresult <- {}
  if (length(symbols) == 2 && 0 %in% symbols && 1 %in% symbols) {
    result <- cbind(result, as.double(column == 1))
    tresult <- cbind(tresult, as.double(tcolumn == 1))
    names <- column.name
  }
  else {
    for (i in 1:(length(symbols))) {
      result <- cbind(result, as.double(column == symbols[i]))
      tresult <- cbind(tresult, as.double(tcolumn == symbols[i]))
    }
    names <- paste(column.name, symbols, sep='_')
  }
  if (column.name != '') {
    colnames(result) <- names
    colnames(tresult) <- names
  }
  
  list(result, tresult)
}

mldr_normalize <- function(mdata) {
  data <- mdata$dataset[c(mdata$attributesIndexes, mdata$labels$index)]
  for (col in mdata$attributesIndexes) {
    if (is.numeric(data[,col])) {
      min_v = min(data[col])
      max_v = max(data[col])
      d <- (max_v - min_v)
      if(is.na(d) || d == 0) {
        d <- 1.0
      }
      data[col] <- (data[col] - min_v) / d
    }
  }
  mldr_from_dataframe(data, mdata$labels$index, mdata$name)
}

#Convert a mldr result to a vector
mresult.as.vector <- function (mresult) {
  measures <- c("Accuracy", "AUC", "AveragePrecision", "Coverage", "FMeasure", "HammingLoss",
    "MacroAUC", "MacroFMeasure", "MacroPrecision", "MacroRecall", "MicroAUC", "MicroFMeasure", "MicroPrecision",
    "MicroRecall", "OneError", "Precision", "RankingLoss", "Recall", "SubsetAccuracy")
  unlist(mresult[measures])
}

change_special_chars <- function(str, replaceWith='.') {
  symbols <- c(' ', "'", '"', '-')
  for (s in symbols)
    str <- gsub(s, replaceWith, str)
  str
}