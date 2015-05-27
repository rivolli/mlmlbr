datasetextractor_execute <- function (mldrdata, path, kfoldmatrix) {
  
  #attrs <- get_attributesOverview(mldrdata);
  #correlation <- get_attributesCorrelationMatrix(mldrdata$dataset);
  #return(get_datasetOverview(mldrdata, attrs));
}

get_datasetOverview <- function (mldrdata, extra=NA) {
  if (is.na(extra)) {
    extra <- get_attributesOverview(mldrdata);
  }
  
  #inst.by.attr: Ratio of number of training instances to the number of attributes
  #num.binary: Number of binary preditive attributes
  #p.binary: Proportion of the binary attributes
  #p.categorical: Proportion of the categorical attributes
  #default.accuracy: Accuracy obtained when using the most frequent labels combination
  #mean.of.means: The mean of all numerical attributes means
  #mean.of.sd: The mean of all numerical attributes standard deviations
  #mean.of.skewness: The mean of all numerical attributes skewness measure
  #mean.of.curtosis: The mean of all numerical attributes kurtosis measure
  #mean.of.entropies: The mean of all discrete attributes simple entropy
  #aac: Average Absolute Correlation between numeric attributes (indicate irrelevante attributes)
  #p.outliers: Proportion of numeric attributes with outliers
  #agr: Average Gain Ration
  #labelset: Number of distinct classes (real labels combinations)
  #ldiversity: Ratio of distinct labelset to the total number of possible label combinations
  #lcard: Label Cardinality
  #lcard.sd: Standard deviation of the label cardinality
  #lcard.skewness: Skewness of label cardinality
  #lcard.kurtosis: Kurtosis of label cardinality
  #ldensity: Label density (Lcard/num.labels)
  #n2, n5, n10, n50: Number of labelset with 2/5/10/50 example and less examples
  #p2, p5, p10, p50: Proportion of labelset with 2/5/10/50 examples and less examples
  #labeln20, ..., labeln500, labelp20, ..., labelp500: The same as n2/p2 ... but with labels and not labelset and multiply by 10
  #poor.labelset: Ratio of labelset with number of examples less than half of the attributes
  #inst.by.labelset: Ratio of the number of instances by labelset
  #overal.complexity: Log of product of instances × labels × features
  #p.uniq: The proportion of label sets which are unique across the total number of examples
  #p.max: The proportion of examples associated with the most frequently occurring labelset
  keys <- c(
    'num.instances', 'num.attributes', 'inst.by.attr', 'num.binary', 'p.binary',
    'p.categorical', 'default.accuracy', 'mean.of.means', 'mean.of.sd', 'mean.of.skewness',
    'mean.of.curtosis', #'mean.of.entropies', 'aac', 'p.outliers', 'agr',
    'num.labels', 'num.labelset', 'num.single.labelset', 'ldiversity', 'lcard', 'lcard.sd', 'lcard.skewness',
    'lcard.kurtosis', 'ldensity', 'n2', 'n5', 'n10', 'n50', 'p2', 'p5', 'p10', 'p50',
    'labeln20', 'labeln50', 'labeln100', 'labeln500', 'labelp20', 'labelp50', 'labelp100', 'labelp500',
    'poor.labelset', 'inst.by.labelset', 'overal.complexity', 'p.uniq', 'p.max'
  );
  overview <- as.list(sapply(keys,function(x) NA));
  overview$num.instances <- mldrdata$measures$num.instances;
  overview$num.attributes <- mldrdata$measures$num.attributes - mldrdata$measures$num.labels;
  overview$inst.by.attr <- overview$num.instances / overview$num.attributes;
  overview$num.binary <- length(which(extra$is.binary)) - mldrdata$measures$num.labels;
  overview$p.binary <- overview$num.binary / overview$num.attributes;
  overview$p.categorical <- length(which(extra$is.ordinal)) / overview$num.attributes;
  
  calcAccuracy <- function (name, t) {
    default.labels <- as.integer(mldrdata$labels$count %in% sort(mldrdata$labels$count)[t:mldrdata$measures$num.labels]);
    Y <- as.integer(unlist(strsplit(name, '')));
    return (length(which(default.labels & Y)) / length(which(default.labels | Y)) * mldrdata$labelset[name]);
  }
  overview$default.accuracy <- max(sapply(1:mldrdata$measures$num.labels, function (t){ 
    sum(unlist(lapply(names(mldrdata$labelsets), calcAccuracy, t))) / overview$num.instances;
  }));
  
  overview$mean.of.means <- mean(extra[complete.cases(extra$mean),"mean"]);
  overview$mean.of.sd <- mean(extra[complete.cases(extra$sd),"sd"]);
  overview$mean.of.skewness <- mean(extra[complete.cases(extra$skewness),"skewness"]);
  overview$mean.of.curtosis <- mean(extra[complete.cases(extra$kurtosis),"kurtosis"]);
  #overview$mean.of.entropies
  #overview$aac
  #overview$p.outliers
  #overview$agr
  overview$num.labels <- mldrdata$measures$num.labels;
  overview$num.labelset <- mldrdata$measures$num.labelsets;
  overview$num.single.labelset <- mldrdata$measures$num.single.labelsets;
  overview$ldiversity <- overview$num.labelset / 2^overview$num.labels;
  overview$lcard <- mldrdata$measures$cardinality;
  lbls <- apply(mldrdata$dataset[,mldrdata$labels$index], 1, sum);
  overview$lcard.sd <- sd(lbls);
  overview$lcard.skewness <- sum((lbls-overview$lcard)^3) / ((length(lbls)-1) * overview$lcard.sd^3);
  overview$lcard.kurtosis <- (sum((lbls-overview$lcard)^4) / ((length(lbls)-1) * overview$lcard.sd^4)) - 3;
  overview$ldensity <- mldrdata$measures$density;
  overview$n2 <- length(which(mldrdata$labelsets <= 2));
  overview$n5 <- length(which(mldrdata$labelsets <= 5));
  overview$n10 <- length(which(mldrdata$labelsets <= 10));
  overview$n50 <- length(which(mldrdata$labelsets <= 50));
  overview$p2 <- overview$n2 / overview$num.labelset;
  overview$p5 <- overview$n5 / overview$num.labelset;
  overview$p10 <- overview$n10 / overview$num.labelset;
  overview$p50 <- overview$n50 / overview$num.labelset;
  overview$labeln20 <- length(which(mldrdata$labels$count < 20));
  overview$labeln50 <- length(which(mldrdata$labels$count < 50));
  overview$labeln100 <- length(which(mldrdata$labels$count < 100));
  overview$labeln500 <- length(which(mldrdata$labels$count < 500));
  overview$labelp20 <- overview$labeln20/ overview$num.labels;
  overview$labelp50 <- overview$labeln50/ overview$num.labels;
  overview$labelp100 <- overview$labeln100/ overview$num.labels;
  overview$labelp500 <- overview$labeln500/ overview$num.labels;
  overview$poor.labelset <- length(which(mldrdata$labelsets < overview$num.attributes/2)) / overview$num.labelset;
  overview$inst.by.labelset <- overview$num.instances / overview$num.labelset;
  overview$overal.complexity <- log10(overview$num.instances * overview$num.attributes * overview$num.labels);
  overview$p.uniq <- length(which(mldrdata$labelsets < 2)) / overview$num.labelset;
  overview$p.max <- max(emotions$labelsets) / overview$num.instances;
  
  #extra
  overview$max.frequency <- mldrdata$measures$max.frequency;
  overview$meanIR <- mldrdata$measures$meanIR;
  return (overview);
}

get_attributesOverview <- function (mldrdata) {
  attrmtx <- do.call("rbind.data.frame", lapply(1:mldrdata$measures$num.attributes, get_attributeDetails, mldrdata=mldrdata));
  rownames(attrmtx) <- colnames(mldrdata$dataset[,1:mldrdata$measures$num.attributes]);
  return(attrmtx);
}

#mldrdata - Data structure
#i - The number/name of attribute (column)
get_attributeDetails <- function (mldrdata, i) {
  rdata <- sort(mldrdata$dataset[complete.cases(mldrdata$dataset[,i]), i]);
  n <- length(rdata);
  
  #is.preditive: 1 for predictive 0 for target attribute
  #is.discrete: 1 for discrete and attribute (binary are also discrete)
  #is.continuos: 1 for continuos attribute
  #is.binary: 1 for binary attribute
  #is.nominal: 1 for nominal attribute
  #is.ordinal: 1 for ordinal attribute
  #num.NA: The number of ausents value
  #is.quantitative: 1 for numeric
  #max.frequency: The count of attribute with more repetitions
  #tmean: Truncate mean removing 10% of borders
  #add: "Absolute average deviation"
  #mad: "Median absolute deviation"
  #iqr: Interquartiles Interval
  keys <- c(
    'is.preditive', 'is.discrete', 'is.continuos', 'is.binary',
    'is.nominal', 'is.ordinal', 'is.quantitative', 'num.NA', 'max.frequency', 'mode',
    'mean', 'tmean', 'median', 'Q1','Q3', 'interval', 'sd', 'add', 'mad', 'iqr',
    'skewness', 'kurtosis'
  );
  row <- as.list(sapply(keys,function(x) NA));
  
   row$is.preditive <- as.integer(!any(mldrdata$labels["index"] == i));
  if (mldrdata$attributes[i] == 'numeric') {
    row$is.discrete <- sum(rdata) == sum(as.integer(rdata));
    row$is.continuos <- !row$is.discrete;
    row$is.binary <- row$is.discrete && !any(rdata < 0 || rdata > 1);    
    row$is.nominal <- row$is.ordinal <- F;
  }
  else {
    row$is.discrete <- row$is.binary <- mldrdata$attributes[i] == '{0,1}';
    row$is.continuos <- F;
    row$is.nominal <- T;
    row$is.ordinal <- F; #TODO see how to determine if attribute data is ordinal
  }
  
  row$num.NA <- length(mldrdata$dataset[,i]) - length(rdata);
  row$is.quantitative <- max(row$is.continuos, row$is.discrete) == 1;
  maxvalue <- -sort(-table(rdata))[1];
  row$max.frequency <- maxvalue[[1]];
  if (row$max.frequency > 1) {
    if (row$is.quantitative) {
      row$mode <- as.numeric(names(maxvalue));
    }
    else {
      row$mode <- names(maxvalue);
    }
  }
  
  if (row$is.quantitative) {
    row$interval <- max(rdata) - min(rdata);
    if (!row$is.binary) {
      row$mean <- mean(rdata);
      
      lim <- as.integer(length(rdata) * .10);
      row$tmean <- mean(rdata[lim:(length(rdata)-lim)]);
      
      row$median <- median(rdata);
      
      row$Q1 <- rdata[as.integer(length(rdata) * .25 + .5)];
      row$Q3 <- rdata[as.integer(length(rdata) * .75 + .5)];
      
      row$sd <- sd(rdata);
      row$add <- sum(rdata - row$mean) / n;
      row$mad <- median(rdata - row$mean);
      row$iqr <- row$Q3 - row$Q1;
      
      row$skewness <- sum((rdata - row$mean) ^ 3) / ((n-1) * row$sd^3);
      row$kurtosis <- (sum((rdata - row$mean) ^ 4) / ((n-1) * row$sd^4)) - 3;
    }
  }
  return (row);
}

#**** Correlations *****
# Only with numeric data
get_attributesCorrelationMatrix <- function (matrix) {
  t <- ncol(matrix);
  mtx <- matrix(ncol=t);
  mtx <- do.call("rbind", lapply(1:t, function (i) {
    idx <- complete.cases(matrix[,i]);
    row <- c();
    for (j in i:t) {
      rdx <- idx & complete.cases(matrix[,j]);
      covariance = sum((matrix[rdx,i]-mean(matrix[rdx,i])) * (matrix[rdx,j]-mean(matrix[rdx,j])))/(length(rdx) - 1);
      row[j] = covariance / (sd(matrix[rdx,i])*sd(matrix[rdx,j]));
    }
    return(row);
  }));
  
  for (i in 1:t) {
    for (j in 1:i) {
      mtx[i,j] <- mtx[j,i];
    }
  }
  
  colnames(mtx) <- colnames(matrix);
  rownames(mtx) <- colnames(matrix);
  
  return(mtx);
}