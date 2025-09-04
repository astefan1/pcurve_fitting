# ==============================================================================
# p-Hacking Strategy: Compute p-curve from simulation output
# ==============================================================================

#' Compute p-curve statistics
#' @description Outputs the fraction of significant p-values in bins of 0-0.01, ..., 0.04-0.05
#' @param ps Vector of p-values
#' @param alpha Significance level
#' @param total.sig TRUE/FALSE Should one value be added to the output indicating the overall fraction of significant p-values
#' @param binwidth How wide are the bins in p-curve (default: 0.01)
#' @export

compute_pcurve <- function(ps, alpha = 0.05, total.sig = FALSE, binwidth = 0.01){

  # Catch errors (if any of the p values are missing, return missing)
  if(all(is.na(ps))) return(rep(NA, 5))

  # Count p-values
  np <- length(ps)

  # How many are significant?
  nsig <- length(ps[ps < alpha])

  # How many % of significant p-values are in each bin?
  perc <- table(cut(ps[ps < alpha], breaks = seq(0, alpha, by = binwidth)))/nsig

  # Return %s (and total %)
  res <- c(unname(perc), switch(total.sig + 1, NULL, nsig))

  return(res)
}
