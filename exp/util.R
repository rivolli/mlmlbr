now <- function () {
  return(as.character(Sys.time()));
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

  mldr_from_dataframe(dataset, mdata$labels$index, names(mdata$attributes))
}

#Remove unique attributes and Remove empty class
remove_unique_attributes <- function (mdata) {
  dataset <- data.frame(row.names=1:mdata$measures$num.instances)
  oldIndex <- numeric(0)
  clsIndex <- numeric(0)
  for (i in 1:mdata$measures$num.attributes) {
    if (length(unique(mdata$dataset[,i])) > 1) {
      idx <- length(oldIndex) + 1
      oldIndex[idx] <- i
      dataset <- cbind(dataset, mdata$dataset[,i])
      if (i %in% mdata$labels$index) clsIndex[length(clsIndex)+1] <- idx
    }
  }

  mldr_from_dataframe(dataset, clsIndex, colnames(mdata$dataset[oldIndex]))
}
