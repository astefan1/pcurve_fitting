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
#' @param prop_Hacker Proportion of p-hackers in the population
#' @param prop_H1 Proportion of H1 in the population
#' @importFrom TruncExpFam rtruncinvgamma
#' @importFrom stats rnorm
#' @export
#' @return Matrix of size iter x 3, with columns being smallest, smallest significant, and first significant p-value

sim.multDVhack <- function(nvar, r, d, prop_Hacker, prop_H1, het = 0, iter = 1000, alpha = 0.05){

  # Draw number of observations from empirical distribution
  nobs.group <- round(TruncExpFam::rtruncinvgamma(n = iter, a=5, b=1905, shape=1.15326986, scale=0.04622745))

  # Draw population effect size from specified distribution and scramble
  effsize <- c(stats::rnorm(n = round(iter*prop_H1), mean = d, sd = het), rep(0, round((1-prop_H1)*iter)))
  effsize <- sample(effsize, iter, replace = FALSE)

  # Simulate as many datasets as desired iterations
  dat <- sapply(1:iter, function(x) .sim.multDV(nobs.group = nobs.group[x], nvar = nvar, r = r, d = effsize[x]))

  # Apply p-hacking procedure to each dataset and extract p-values
  if(prop_Hacker == 0){
    ps <- unname(sapply(dat, .multDVhack, dvs = 2, group = 1, alpha = alpha, USE.NAMES = FALSE))
  } else if (prop_Hacker == 1){
    ps <- unname(sapply(dat, .multDVhack, dvs = c(2:(nvar+1)), group = 1, alpha = alpha, USE.NAMES = FALSE))
  } else {
    ps <- cbind(
      # P-Hacked part of the datasets
      unname(sapply(dat[1:round(prop_Hacker*iter)], .multDVhack, dvs = c(2:(nvar+1)), group = 1, alpha = alpha, USE.NAMES = FALSE)),
      # Non-p-hacked part of the datasets
      unname(sapply(dat[(round(prop_Hacker*iter+1):iter)], .multDVhack, dvs = 2, group = 1, alpha = alpha, USE.NAMES = FALSE))
    )
  }

  return(t(ps))

}

