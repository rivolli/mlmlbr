conf.matrix <- function (predict, real, positive.value=1, negative.value=0) {
  mtx <- NULL;
  mtx$n <- length(real);
  mtx$VP <- length(which(real[which(predict == positive.value)] == positive.value));
  mtx$FP <- length(which(real[which(predict == positive.value)] == negative.value));
  mtx$FN <- length(which(real[which(predict == negative.value)] == positive.value));
  mtx$VN <- length(which(real[which(predict == negative.value)] == negative.value));
  return(mtx);
}

#Taxa de erro ou custo do classificador
metrics.err <- function (mtx) {
  return ((mtx$FP + mtx$FN) / mtx$n);
}

#Acuracia Geral do classificador
metrics.accuracy <- function (mtx) {
  return ((mtx$VP + mtx$VN) / mtx$n);
}

#Acuracia Balanceada (Média das acurárias positivas e negativas), utilizada para classes desbalanceadas
metrics.balancedAccuracy <- function (mtx) {
  P <- (mtx$VP + mtx$FN)
  N <- (mtx$FP + mtx$VN)

  if (P == 0) {
     if (N == 0) {
        result <- 0
    }
    else {
      result <- mtx$VN / N
    }
  }
  else if (N == 0) {
    result <- mtx$VP / P
  }
  else {
    result = ((mtx$VP / P) + (mtx$VN / N)) / 2
  }
  result
}

#Taxa de erro da classe positiva (classe positiva incorretamente classificada)
metrics.TFN <- function (mtx) { #Taxa de Falso Negativo
  if ((mtx$FN + mtx$VP) == 0) {
    return (0);
  }
  else {
    return (mtx$FN / (mtx$FN + mtx$VP));
  }
}

#Taxa de erro da classe negativa (classe negativa incorretamente classificada)
metrics.TFP <- function (mtx) { #Taxa de Falso Positivo
  if ((mtx$FP + mtx$VN) == 0) {
    return (0);
  }
  else {
    return (mtx$FP / (mtx$FP + mtx$VN));
  }
}

#Precisao (exemplos positivos classificados corretamente entre todos preditos como positivos)
metrics.precision <- function (mtx) {
  if ((mtx$VP + mtx$FP) == 0) {
    return (0);
  }
  else {
    return (mtx$VP / (mtx$VP + mtx$FP));
  }
}

#Sensibilidade ou Revocacao (Taxa de Verdadeiro Positivo)
metrics.recall <- function (mtx) {
  if ((mtx$VP + mtx$FN) == 0) {
    return (0);
  }
  else {
    return (mtx$VP / (mtx$VP + mtx$FN));
  }
}

#Especificidade - Taxa de acerto da classe negativa (complemento da TFP)
metrics.specificity <- function (mtx) {
  if ((mtx$VN + mtx$FP) == 0) {
    return (0);
  }
  else {
    return (mtx$VN / (mtx$VN + mtx$FP));
  }
}

#Generic F measure
metrics.Fmeasure <- function (mtx, w) {
  precision <- metrics.precision(mtx);
  recall <- metrics.recall(mtx);
  if ((recall + w * precision) == 0) {
    return (0);
  }
  else {
    return ((w+1) * recall * precision / (recall + w * precision));
  }
}

metrics.F1 <- function (mtx) {
  return (metrics.Fmeasure(mtx, 1));
}

binary.evaluate <- function (mtx) {
  return(list(
    Accuracy    = metrics.accuracy(mtx),
    TFN         = metrics.TFN(mtx),
    TFP         = metrics.TFP(mtx),
    Precision   = metrics.precision(mtx),
    Recall      = metrics.recall(mtx),
    Specificity = metrics.specificity(mtx),
    F1          = metrics.accuracy(mtx),
    BalancedAccuracy = metrics.balancedAccuracy(mtx)
  ));
}

binary.evaluate.complete <- function (predict, real, probs, positive.value=1, negative.value=0){
  if (nlevels(real) != 2) {
    cat(">>> Different NLEVELS", nlevels(real), "\n")
  }

  measures <- binary.evaluate(conf.matrix(predict, real))

  #pred <- prediction(probs, real)
  #measures$AUC <- as.numeric(performance(pred, "auc")@y.values)

  measures$AUC <- tryCatch(acc.auc(probs, real), error=function(err) 0) #when AUC is not possible the result is 0

  as.data.frame(measures)
}