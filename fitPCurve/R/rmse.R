# ==============================================================================
# Compute root mean squared error (extent of deviation between two p-curves)
# ==============================================================================

#' Compute RMSE between two p-curves
#' @description Calculates square root of mean of squared differences between two vectors
#' @param v1 First vector (original)
#' @param v2 Second vector (fitted values)
#' @export

rmse <- function(v1, v2){
  sqrt(mean((v1-v2)^2))
}
