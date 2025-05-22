# ==============================================================================
# p-Hacking Strategy: Selective Reporting of the Dependent Variable
# ==============================================================================

#' Simulate dataset with multiple dependent variables
#' @description Outputs data frame with a grouping variable and multiple correlated dependent variables
#' @param nobs.group Vector giving number of observations per group
#' @param nvar Number of dependent variables in the data frame
#' @param r Desired correlation between the dependent variables (scalar)
#' @param d Desired population effect size (standardized mean difference between grouping variable levels)

.sim.multDV <- function(nobs.group, nvar, r, d){

  # Observations per group
  if(length(nobs.group) == 1) nobs.group <- rep(nobs.group, 2)

  # Generate group vector
  group <- c(rep(0, nobs.group[1]), rep(1, nobs.group[2]))

  # Generate dependent variables
  dvs <- .rmultcor(nobs = sum(nobs.group), nvar = nvar, r = r)

  # Introduce population effect size
  dvs <- apply(dvs, 2, function(x) x + group*d)

  # Generate data frame
  res <- cbind(group, dvs)

  return(res)
}

