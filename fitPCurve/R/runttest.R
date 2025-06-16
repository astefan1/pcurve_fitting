#' Compute independent-samples t-test
#' @description Computes p-value from two-sided t-test from sample
#' @param y Vector of y values
#' @param group Vector indicating group membership as 0 or 1
#' @importFrom stats var
#' @importFrom stats pt

.runttest <- function(y, group){

  y1 <- y[group==1]
  y2 <- y[group==0]
  sderr <- sqrt((var(y1)+var(y2))/2)*sqrt(2/length(y1))
  tval <- abs((mean(y1)-mean(y2))/sderr)

  (1-pt(tval, length(y)-2))*2

}
