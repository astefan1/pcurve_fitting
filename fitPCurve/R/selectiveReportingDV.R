# ==============================================================================
# p-Hacking Strategy: Selective Reporting of the Dependent Variable
# ==============================================================================

#' Simulate one dataset with multiple dependent variables
#' @description Outputs data frame with a grouping variable and multiple correlated dependent variables
#' @param nobs.group Integer giving number of observations per group
#' @param nvar Number of dependent variables in the data frame
#' @param r Desired correlation between the dependent variables (scalar)
#' @param d Desired population effect size (standardized mean difference between grouping variable levels)

.sim.multDV <- function(nobs.group, nvar, r, d){

  # Generate group vector
  group <- c(rep(0, nobs.group), rep(1, nobs.group))

  # Generate dependent variables
  dvs <- .rmultcor(nobs = nobs.group*2, nvar = nvar, r = r)

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
#' @param alpha Significance level of the t-test

.multDVhack <- function(df, dvs, group, alpha = 0.05){

  # Prepare data frame
  dvs_matrix <- as.matrix(df[, dvs], ncol = length(dvs))
  group <- df[, group]

  # Apply t-test function to different DVs
  ps <- apply(dvs_matrix, 2, function(x) .runttest(y=x, group=group))

  # Select final p-hacked p-value based on strategy
  p.final <- .selectpvalue(ps = ps, alpha = alpha)

  return(p.final)

}

#' Simulate p-Hacking with multiple dependent variables
#' @description Outputs a vector containing the p-hacked p-values
#' @param nvar Number of dependent variables (columns) in the data frame
#' @param r Desired correlation between the dependent variables (scalar)
#' @param d Population effect size (standardized mean difference between grouping variable levels)
#' @param het Effect size heterogeneity (standard deviation of effect sizes in the population, set to zero if no heterogeneity is modeled)
#' @param iter Number of simulation iterations
#' @param alpha Significance level of the t-test (default: 0.05)
#' @importFrom TruncExpFam rtruncinvgamma
#' @importFrom stats rnorm
#' @export
#' @return Matrix of size iter x 3, with columns being smallest, smallest significant, and first significant p-value

sim.multDVhack <- function(nvar, r, d, het = 0, iter = 1000, alpha = 0.05){

  # Draw number of observations from empirical distribution
  nobs.group <- round(TruncExpFam::rtruncinvgamma(n = iter, a=5, b=1905, shape=1.15326986, scale=0.04622745))

  # Draw population effect size from specified distribution
  effsize <- stats::rnorm(n = iter, mean = d, sd = het)

  # Simulate as many datasets as desired iterations
  dat <- sapply(1:iter, function(x) .sim.multDV(nobs.group = nobs.group[x], nvar = nvar, r = r, d = effsize[x]))

  # Apply p-hacking procedure to each dataset and extract p-values
  ps <- unname(sapply(dat, .multDVhack, dvs = c(2:(nvar+1)), group = 1, alpha = alpha, USE.NAMES = FALSE))

  return(t(ps))

}

