# ==============================================================================
# Compute chi2 goodness of fit statistic
# ==============================================================================

#' Compute Chi2 goodness of fit statistic between two p-curves
#' @description Calculates Chi squared statistic between two vectors
#' @param v1 First vector of probabilities (original)
#' @param v2 Second vector of probabilities (fitted values)
#' @param n Total sample size
#' @export

chi2 <- function(v1, v2, n){
  stopifnot(round(sum(v1), 6) == 1)
  stopifnot(round(sum(v2), 6) == 1)
  sum((v1-v2)^2/v2)*n
}
