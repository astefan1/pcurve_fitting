#' Select smallest, smallest significant, and first significant p-value from a vector of p-hacked p-values
#' @description Takes a vector of p-values and selects the smallest, first significant, or smallest significant p-value.
#' @param ps Vector of p values
#' @param alpha Significance level (default: 0.05)

.selectpvalue <- function(ps, alpha){

  p.final <- rep(NA, 3) # 1: smallest, 2: smallest significant, 3: first significant
  p.orig <- ps[1]

  p.final[1] <- min(ps)

  if(min(ps) < alpha){
    p.final[2] <- min(ps)
    p.final[3] <- ps[which(ps < alpha)[1]]
  } else {
    p.final[2:3] <- p.orig
  }

  return(p.final)

}
