get_kfoldsIndexes <- function (mldrdata, k=10) {
  t <- nrow(mldrdata$dataset);
  if ((t %% k) == 0) {
    return (matrix(sample(t), k));
  }
  else {
    return (matrix(c(sample(t), rep(NA, k - t %% k)), k));
  }
}