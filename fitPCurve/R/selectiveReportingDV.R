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

#' P-Hacking function for multiple dependent variables
#' @description Outputs a p-hacked p-value
#' @param df Data frame with one group variable and multiple dependent variables
#' @param dvs Vector defining the DV columns (will be checked in given order)
#' @param group Scalar defining grouping column
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alternative Direction of the t-test ("two.sided", "less", "greater")
#' @param alpha Significance level of the t-test
#' @importFrom stats t.test

.multDVhack <- function(df, dvs, group, strategy = "firstsig", alternative = "two.sided", alpha = 0.05){

  # Prepare data frame
  dvs <- as.matrix(df[, dvs], ncol = length(dvs))
  group <- df[, group]

  # Define t-test function
  ttestfun <- function(x){
    stats::t.test(x ~ group, var.equal = TRUE, alternative = alternative)$p.value
  }

  # Re-apply t-test function to different DVs
  ps <- apply(dvs, 2, ttestfun)

  # Select final p-hacked p-value based on strategy
  p.final <- .selectpvalue(ps = ps, strategy = strategy, alpha = alpha)

  return(p.final)

}

#' Simulate p-Hacking with multiple dependent variables
#' @description Outputs a vector containing the p-hacked p-values
#' @param nobs.group Vector giving number of observations per group
#' @param nvar Number of dependent variables (columns) in the data frame
#' @param r Desired correlation between the dependent variables (scalar)
#' @param d Desired population effect size (standardized mean difference between grouping variable levels)
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param iter Number of simulation iterations
#' @param alternative Direction of the t-test ("two.sided", "less", "greater")
#' @param alpha Significance level of the t-test (default: 0.05)
#' @export

sim.multDVhack <- function(nobs.group, nvar, r, d, strategy = "firstsig", iter = 1000, alternative = "two.sided", alpha = 0.05){

  # Simulate as many datasets as desired iterations
  dat <- replicate(iter, .sim.multDV(nobs.group = nobs.group, nvar = nvar, r = r, d = d))

  # Apply p-hacking procedure to each dataset and extract p-values
  ps <- apply(dat, 3, .multDVhack, dvs = c(2:(nvar+1)), group = 1, strategy = strategy, alternative = alternative, alpha = alpha)

  return(ps)

}

