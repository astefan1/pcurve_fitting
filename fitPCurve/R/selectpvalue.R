#' Select a p-value from a vector of p-hacked p-values
#' @description Takes a vector of p-values and selects the smallest, first significant, or smallest significant p-value.
#' @param ps Vector of p values
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alpha Significance level (default: 0.05)

.selectpvalue <- function(ps, strategy, alpha){

  p.final <- NA
  p.orig <- ps[1]

  # Select smallest significant p-value
  if(strategy == "smallest.sig"){

    if(min(ps) < alpha){
      p.final <- min(ps)
    } else {
      p.final <- p.orig
    }

    # Select first significant p-value
  } else if (strategy == "firstsig") {

    if(min(ps) < alpha){
      p.final <- ps[which(ps < alpha)[1]]
    } else {
      p.final <- p.orig
    }

    # Select smallest p-value
  } else if (strategy == "smallest") {
    p.final <- min(ps)
  }

  return(p.final)

}
