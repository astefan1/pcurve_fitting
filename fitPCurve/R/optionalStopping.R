# ==============================================================================
# p-Hacking Strategy: Selective Reporting of the Dependent Variable
# ==============================================================================

# First simulate dataset of p-values under H1

nMax <- ceiling(TruncExpFam::qtruncinvgamma(p=0.99, a=5, b=1905, shape=1.15326986, scale=0.04622745))
groups <- 2 # independent-samples t-test
iter <- 10000
ds <- seq(0.1, 0.8, by = 0.1)
het <- seq(0, 0.4, by = 0.1)
ES <- expand.grid(ds, het)
ESlist <- apply(ES, 1, function(x) rnorm(iter, x[1], x[2]))

simdata0 <- array(rnorm(prod(nMax, groups, iter)), dim=c(nMax, groups, iter)) # draw from standard normal
simdata <- array(NA, dim=c(nMax, groups, iter)) # placeholder for each effect size
simres <- array(NA, dim=c(nMax, iter, ncol(ESlist))) # this is where the p-values go

# Sequentially compute t and p values for different effect sizes
for(i in 1:ncol(ESlist)){

  print(i)
  simdata <- simdata0

  # Add effect size to one group
  for(j in 1:iter){
    simdata[, 1, j] <- simdata0[, 1, j] + ESlist[j, i]
  }

  # 1:N
  seqOneToNMax <- seq(1:nMax)
  # cumulative mean difference
  X1minX2 <- apply(simdata, 3, function(x) (cumsum(x[,1])/seqOneToNMax)-(cumsum(x[,2])/seqOneToNMax))
  # cumulative variance
  Var1 <- apply(simdata, 3, function(x) cumvar(x[,1]))
  Var2 <- apply(simdata, 3, function(x) cumvar(x[,2]))
  # cumulative pooled standard deviation
  spool <- sapply(1:iter, function(x) sqrt(((seqOneToNMax-1)*Var1[,x] + (seqOneToNMax-1)*Var2[,x])/(seqOneToNMax+seqOneToNMax-2)))

  # cumulative t-value calculation
  denom <- sapply(1:iter, function(x) spool[,x]*sqrt((1/seqOneToNMax) + (1/seqOneToNMax))) # t value denominator
  tval <- X1minX2/denom # t-value

  # cumulative p-value calculation
  pval <- apply(tval, 2, function(x) (1-pt(abs(x), 2*seqOneToNMax-2))*2) # p-value
  simres[,,i] <- pval
}

optionalStopping <- function(simres, nmin, nmax, stepsize, d, het, alpha, ES){
  peeks <- seq(nmin, nmax, by = stepsize)
  whichES <- which(ES[1] == d && ES[2] == het)
  pvalSmall <- simres[peeks,,whichES]
  pvals <- apply(pvalSmall, 2, function(x) ifelse(any(x < alpha), x[which(x < 0.05)[1]], tail(x, 1)))
}
