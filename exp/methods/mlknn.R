# R Code
# 14/04/2015
# A. Rivolli, A. C.P.L.F. Carvalho, 2015

#' @title Multi-Label K-nn algorithm for R
#' @describeIn Zhang, M. L., & Zhou, Z. H. (2007). ML-KNN: A lazy learning approach to multi-label learning. Pattern Recognition, 40(7), 2038–2048. http://doi.org/10.1016/j.patcog.2006.12.019
#' @param train Mldr data of training set cases
#' @param test Mldr data of test instances cases
#' @param k The number of neighbours considered
#' @param s Smoothing parameter (default is 1)
#' @return A dataset contains predition, ranking and results
#' @export
mlknn <- function(train, test, k=1, s=1) {
  Prior <- mlknn.priorProbabilities(as.matrix(train$dataset[,train$labels$index]), s);
  Post  <- mlknn.posteriorProbabilities(train, k, s);
  
  #Compute Yt and Rt (labels and ranking)
  tsize <- test$measures$num.instances;
  yt <- rt <- as.data.frame(matrix(nrow=tsize, ncol=train$measures$num.labels));
  colnames(yt) <- names(train$attributes[train$labels$index]);
  colnames(rt) <- colnames(yt);
  for (t in 1:tsize) {
    distmtx = mlknn.distanteMatrix(rbind(train$dataset[,train$attributesIndexes], test$dataset[t, train$attributesIndexes]));
    Nt <- mlknn.N(distmtx, nrow(distmtx), k);
    lidx <- 1;
    for (l in train$labels$index) {
      Ctl <- sum(train$dataset[Nt,l]) + 1;
      R1 <- Prior[1, lidx] * Post[[1]][lidx, Ctl];
      R0 <- Prior[2, lidx] * Post[[2]][lidx, Ctl];
      yt[t,lidx] = if (max(c(R1, R0)) == R1) 1 else 0;
      rt[t, lidx] <- R1 / (R1 + R0); 
      lidx <- lidx + 1;
    }
  }
  
  #Evaluate
  dataresults = mldr_evaluate(test, as.matrix(yt));
  return(list(predition=yt, ranking=rt, results=dataresults));
}
 
#' @title Computing the posterior probabilities P(Elj|Hlb)
#' @param ds Mldr data of training set cases
#' @param k The number of neighbours considered
#' @param s Smoothing parameter        
#' @return List of matrix of probabilites. list(PEH1, PEH0)
#'    PEHb row is a label and each column is sigma (k + 1 values)
#' @export
mlknn.posteriorProbabilities <- function (ds, k, s=1) {
  distances = mlknn.distanteMatrix(ds$dataset[,ds$attributesIndexes]);
  m <- ds$measures$num.instances;
  kplus <- k + 1;
  
  #each row is a label and each column is sigma (k + 1 values)
  PEH0 <- PEH1 <- as.data.frame(matrix(nrow=ds$measures$num.labels, ncol=kplus));
  rownames(PEH0) <- names(ds$attributes[ds$labels$index]);
  rownames(PEH1) <- rownames(PEH0);
  
  for (l in 1:ds$measures$num.labels) {
    lidx <- ds$labels$index[l];
    c0 <- c1 <- rep(0, kplus);
    for (i in 1:m) {
      sigma = sum(ds$dataset[mlknn.N(distances, i, k),lidx]) + 1; #+ 1 because is {0,1,2,...,k}
      if (ds$dataset[i,lidx] == 1) {
        c1[sigma] <- c1[sigma] + 1;
      }
      else {
        c0[sigma] <- c0[sigma] + 1;
      }
    }
    
    sc0 <- sum(c0);
    sc1 <- sum(c1);
    for (j in 1:kplus) { #+ 1 because is {0,1,2,...,k} and indexes is 1 based
      PEH1[l,j] <- (s+c1[j]) / (s * kplus + sc1);
      PEH0[l,j] <- (s+c0[j]) / (s * kplus + sc0);
    }
  }
  
  return(list(PEH1, PEH0));
}

#' @title Computing the distance matrix
#' @param data Matrix or Dataframe with numerical data to calculate the distance
#' @return Martrix with nrow(list), ncol(list) distance
#' @export
mlknn.distanteMatrix <- function (data) {
   return(as.matrix(dist(as.matrix(data))));
}

#' @title Find the K elements nearest from xi instance
#' @param distances Distance matrix calculated
#' @param xi Row number of base element
#' @param k The number of nearest elements
#' @return A vector with k indexes of nearest elements from xi
#' @export
mlknn.N <- function (distances, xi, k) {
   return(as.integer(names(sort(distances[xi,-1*xi])[1:k])));
}

#' @title Computing the prior probabilities P(Hlb)
#' @param labelset Matrix or Dataframe with only labels
#' @param s Smoothing parameter        
#' @return Dataframe with 2 rows and |labels| columns
#'   The first row contains Hl1 probabilities for labels
#'   The second row contains Hl0 probabilites for labels
#' @export
mlknn.priorProbabilities <- function (labelset, s=1) {
  #calculate the probabilities
  calc_probability1 <- function (list, s=1) {
    return((s + sum(list))/(s * 2 + length(list))); 
  }
  calc_probablitity0 <- function (list) {
    return(1 - list);
  }
  
  #Create dataframe and set values
  probs <- as.data.frame(matrix(nrow = 2, ncol = ncol(labelset)));
  names(probs) <- colnames(labelset);
  
  probs[1,] <- apply(labelset, 2, calc_probability1, s);
  probs[2,] <- lapply(probs[1,], calc_probablitity0);
  
  return(probs);
}