# ==============================================================================
# p-Hacking Strategy: Compute p-curve from simulation output
# ==============================================================================

#' Compute p-curve statistics
#' @description Outputs the fraction of significant p-values in bins of 0-0.01, ..., 0.04-0.05
#' @param ps Vector of p-values
#' @param alpha Significance level
#' @param k_sig TRUE/FALSE Should one value be added to the output indicating the overall fraction of significant p-values
#' @param binwidth How wide are the bins in p-curve (default: 0.01)
#' @export

compute_pcurve <- function(ps, alpha = 0.05, k_sig = FALSE, binwidth = 0.01){

  # Catch errors (if any of the p values are missing, return missing)
  if(all(is.na(ps))) return(rep(NA, 5))

  # Count p-values
  np <- sum(!is.na(ps))

  # How many are significant?
  nsig <- sum(ps < alpha)

  # How many % of significant p-values are in each bin?
  perc <- table(cut(ps[ps < alpha], breaks = seq(0, alpha, by = binwidth), right=FALSE))/nsig
  
  stopifnot(round(sum(perc), 6)==1)

  # Return %s (and total %)
  res <- c(
    unname(perc), 
    switch(k_sig + 1, NULL, nsig), 
    switch(k_sig + 1, NULL, np))

  names(res) <- c(
    paste0("p", 1:5), 
    switch(k_sig + 1, NULL, "k_sig"), 
    switch(k_sig + 1, NULL, "k"))

  return(res)
}
