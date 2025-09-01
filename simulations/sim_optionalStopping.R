# ==============================================================================
# Optional Stopping Simulation
# ==============================================================================

# This script calculates the p-value sequentially after each observation 10,000
# times in each simulation condition

library(TruncExpFam)

# First simulate dataset of p-values under H1

nMax <- ceiling(TruncExpFam::qtruncinvgamma(p=0.99, a=5, b=1905, shape=1.15326986, scale=0.04622745)) # maximum N per group = 99th percentile of empirical distribution
groups <- 2 # independent-samples t-test
iter <- 10000
ds <- seq(0, 0.8, by = 0.1)
het <- seq(0, 0.4, by = 0.2)
ES <- expand.grid(ds, het)
set.seed(111092)
ESlist <- apply(ES, 1, function(x) rnorm(iter, x[1], x[2])) # each row in ES (ES+het combi) is a column with iter elements in ESlist

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

# Then, compute p-curves for different scenarios

prop_Hacker = c(0.1, seq(0.25, 1, by = 0.25)) # proportion of researchers practicing p-hacking
prop_H1 = c(0.1, 0.2, 0.3, 0.5, 0.7) # proportion of true H1 effects
nmin <- c(6, 12, 23, 222, 444, 532) # based on empirical quantiles of Marzalek (1%, 50%, 95%, 99% and half of each)
nmax <- c(12, 23, 222, 444, 532, 763)
stepsize <- c(1, 5, 10, 50, 100)
ds <- seq(0, 0.8, by = 0.1)
het <- seq(0, 0.4, by = 0.2)

conditions <- expand.grid(prop_Hacker, prop_H1, nmin, nmax, stepsize, ds, het)
colnames(conditions) <- c("prop_Hacker", "prop_H1", "nmin", "nmax", "stepsize", "d", "het")

# Delete superfluous conditions
conditions <- conditions[conditions$nmin < conditions$nmax,] # nmin > nmax
conditions <- conditions[conditions$stepsize < (conditions$nmax-conditions$nmin), ] # stepsize > than diff between min and max

