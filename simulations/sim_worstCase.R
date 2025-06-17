# ==============================================================================
# Worst Case Scenario: No effect, only p-hacking (selective reporting of DV)
# ==============================================================================

library(doParallel)

# Final simulation conditions
# nvar <- seq(2, 200, by = 2)
# r <- seq(0, 0.9, by = 0.1)
# d <- 0
# iter <- 10000
# alpha <- 0.05

# How many cores does your CPU have?
n_cores <- detectCores()
n_cores

# Do a limited simulation for testing
nvar <- c(2, 50, 200)
r <- c(0, 0.5)
d <- 0
iter <- 1000
alpha <- 0.05

# Create object naming conditions
conditions <- expand.grid(nvar, r, d, iter, alpha, stringsAsFactors = FALSE)
colnames(conditions) <- c("nvar", "r", "d", "iter", "alpha")

# Create results object (3-dim array: (1) condition, (2) pcurve %s, (3) selection method)
simresArray <- array(data = NA, dim=c(nrow(conditions), 5, 3))

a <- Sys.time()

#cl <- parallel::makeCluster(n_cores-2)
cl <- parallel::makeCluster(1)
doParallel::registerDoParallel(cl)

foreach(i=1:nrow(conditions)) %dopar% {
  # the library call must be in the loop so that all workers have the functions
  library(fitPCurve)
  ps <- sim.multDVhack (nvar=conditions[i,1],
                        r=conditions[i,2],
                        d=conditions[i,3],
                        iter = conditions[i,4],
                        alpha = conditions[i,5]
                        )
  simresArray[i, , 1] <- compute_pcurve(ps[,1])
  simresArray[i, , 2] <- compute_pcurve(ps[,2])
  simresArray[i, , 3] <- compute_pcurve(ps[,3])
}

parallel::stopCluster(cl)
b <- Sys.time()-a
b

# Merge 3rd dimension of results array into strategy condition in results dataframe 
simresDF <- rbind(cbind(conditions, strategy="smallest", simresArray[,,1]),
                  cbind(conditions, strategy="smallest.sig", simresArray[,,2]),
                  cbind(conditions, strategy="firstsig", simresArray[,,3]))
colnames(simresDF)[7:11] <- paste0("p", 1:5)

write.csv(simresDF, "../simulations/sim-results/worstCase.csv")
