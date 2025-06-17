# ==============================================================================
# Worst Case Scenario: No effect, only p-hacking (selective reporting of DV)
# ==============================================================================

library(doParallel)

# How many cores does your CPU have?
n_cores <- detectCores()
n_cores

# Do a limited simulation for testing
nvar <- c(2, 50, 200)
r <- c(0, 0.5)
d <- 0
strategy <- c("firstsig", "smallest", "smallest.sig")
iter <- 1000
alternative <- "two.sided"
alpha <- 0.05

simres <- expand.grid(nvar, r, d, strategy, iter, alternative, alpha, stringsAsFactors = FALSE)
colnames(simres) <- c("nvar", "r", "d", "strategy", "iter", "alternative", "alpha")
simres[, c(8:12)] <- NA
colnames(simres[, c(8:12)]) <- paste0("p", c(1:5))

a <- Sys.time()

#cl <- parallel::makeCluster(n_cores-2)
cl <- parallel::makeCluster(1)
doParallel::registerDoParallel(cl)

foreach(i=1:nrow(simres)) %dopar% {
  # the library call must be in the loop so that all workers have the functions
  library(fitPCurve)
  ps <- sim.multDVhack (nvar=simres[i,1],
                        r=simres[i,2],
                        d=simres[i,3],
                        strategy = simres[i,4],
                        iter = simres[i,5],
                        alternative = simres[i,6],
                        alpha = simres[i,7]
                        )
  simres[i,8:12] <- compute_pcurve(ps)

}
parallel::stopCluster(cl)
b <- Sys.time()-a
b


