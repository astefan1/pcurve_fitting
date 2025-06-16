#' Compute independent-samples t-test
#' @description Computes p-value from two-sided t-test from sample

.runttest <- function(y, group){

  y1 <- y[group==1]
  y2 <- y[group==0]
  sderr <- sqrt((var(y1)+var(y2))/2)*sqrt(2/length(y1))
  tval <- abs((mean(y1)-mean(y2))/sderr)

  (1-pt(tval, length(y)-2))*2

}
