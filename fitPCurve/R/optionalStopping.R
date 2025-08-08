# ==============================================================================
# p-Hacking Strategy: Selective Reporting of the Dependent Variable
# ==============================================================================

# First simulate dataset of p-values

nMax <- ceiling(TruncExpFam::qtruncinvgamma(p=0.99, a=5, b=1905, shape=1.15326986, scale=0.04622745))
groups <- 2 # independent-samples t-test
iter <- 10000
ds <- seq(0, 0.1, 0.8, by = 0.1)

simdata0 <- array(rnorm(prod(nMax, groups, iter)), dim=c(nMax, groups, iter)) # draw from standard normal
simres <- array(NA, dim=c(nMax, iter, length(ds))) # this is where the p-values go

# Sequentially compute t and p values for different effect sizes
for(i in seq_along(ds)){
  print(i)
  simdata[, 1, ] <- simdata0[, 1, ] + ds[i]
  seqOneToNMax <- seq(1:nMax) # 1:N
  X1minX2 <- apply(simdata, 3, function(x) (cumsum(x[,1])/seqOneToNMax)-(cumsum(x[,2])/seqOneToNMax)) #cum mean diff
  Var1 <- apply(simdata, 3, function(x) cumvar(x[,1])) # cum var 1
  Var2 <- apply(simdata, 3, function(x) cumvar(x[,2])) # cum var 2
  spool <- sapply(1:iter, function(x) sqrt(((seqOneToNMax-1)*Var1[,x] + (seqOneToNMax-1)*Var2[,x])/(seqOneToNMax+seqOneToNMax-2))) # cum pooled SD
  denom <- sapply(1:iter, function(x) spool[,x]*sqrt((1/seqOneToNMax) + (1/seqOneToNMax))) # t value denominator
  tval <- X1minX2/denom # t-value
  pval <- apply(tval, 2, function(x) (1-pt(abs(x), 2*seqOneToNMax-2))*2) # p-value
  simres[,,i] <- pval
}

optionalStopping <- function(simres, nmin, nmax, stepsize, ES, alpha){
  peeks <- seq(nmin, nmax, by = stepsize)
  whichES <- which(seq(0.1, 0.8, by = 0.1) == ES)
  pvalSmall <- simres[peeks,,whichES]
  pvals <- apply(pvalSmall, 2, function(x) ifelse(any(x < alpha), x[which(x < 0.05)[1]], tail(x, 1)))
}
