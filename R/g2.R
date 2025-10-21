# ==============================================================================
# Compute G-squared (Goodness of fit statistic)
# ==============================================================================

g2 <- function(reference, comparison, n, tol = 1e-10){
  
  if (is.data.frame(comparison)) comparison <- as.matrix(comparison)  # ensures matrix methods work for data.frame input
  
  
  
}