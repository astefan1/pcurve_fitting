# ==============================================================================
# p-Hacking Strategy: Selective Reporting of the Dependent Variable
# ==============================================================================

# This function works as parser of the big optional stopping simulation object
# created in sim_optionalStopping.R .

#' Run optional stopping and return p-curve
#' @param simres Simulation result created in sim_optionalStopping.R (array: nMax x iter x ncol(ES))
#' @param ES Corresponding effect size object (expand.grid(ds, het))
#' @param prop_Hacker Proportion of p-hackers in the population
#' @param prop_H1 Proportion of H1 in the population
#' @param nmin Minimum sample size per group
#' @param nmax Maximum sample size per group
#' @param stepsize Every how many participants do you check?
#' @param d Effect size
#' @param het Heterogeneity
#' @param alpha Significance level
#' @importFrom TruncExpFamily rtruncinvgamma
#' @export

optionalStopping <- function(simres, ES, prop_Hacker, prop_H1, nmin, nmax, stepsize, d, het, alpha = 0.05){

  # Compile a dataset of p-values with the right proportions of H0 and H1
  if(prop_H1 == 0){
    whichES <- which(ES[,1] == 0 & ES[,2] == 0)
    selectPs <- simres[,,whichES]
  } else if(prop_H1 == 1){
    whichES <- which(round(ES[,1],5) == d & round(ES[,2],5) == het) #rounding necessary due to floating point issues
    selectPs <- simres[,,whichES]
  } else {
    iterH1 <- round(prop_H1*iter)
    iterH0 <- iter-iterH1
    whichESH1 <- which(round(ES[,1],5) == d & round(ES[,2],5) == het)
    whichESH0 <- which(ES[,1] == 0 & ES[,2] == 0)
    selectPs <- cbind(simres[,1:iterH1,whichESH1], simres[,1:iterH0,whichESH0])
    selectPs <- selectPs[,sample(1:ncol(selectPs), ncol(selectPs), replace = FALSE)] # scramble columns so that H0 and H1 effects are mixed
  }

  # Split p-value data into hackers and non-hackers and select p-values accordingly
  if(prop_Hacker == 0){
    nmax <- round(TruncExpFam::rtruncinvgamma(n=iter, a=5, b=nmax, shape=1.15326986, scale=0.04622745))
    ps <- sapply(1:iter, function(x) selectPs[nmax[x], x])
  } else if(prop_Hacker == 1){
    peeks <- seq(nmin, nmax, by = stepsize)
    pvalSmall <- selectPs[peeks, ]
    ps <- apply(pvalSmall, 2, function(x) ifelse(any(x < alpha), x[which(x < 0.05)[1]], tail(x, 1))) # this does the stopping
  } else{
    # Hackers
    iterHack <- round(prop_Hacker*iter)
    peeks <- seq(nmin, nmax, by = stepsize)
    pvalSmall <- selectPs[peeks, 1:iterHack]
    ps <- apply(pvalSmall, 2, function(x) ifelse(any(x < alpha), x[which(x < 0.05)[1]], tail(x, 1))) # this does the stopping
    # Non-hackers
    iterNoHack <- iter-iterHack
    nmax <- round(TruncExpFam::rtruncinvgamma(n=iterNoHack, a=5, b=nmax, shape=1.15326986, scale=0.04622745))
    pvalSmall <- selectPs[, (iterHack+1):iter]
    ps <- c(ps, sapply(1:iterNoHack, function(x) pvalSmall[nmax[x], x]))
  }

  return(ps)

}
