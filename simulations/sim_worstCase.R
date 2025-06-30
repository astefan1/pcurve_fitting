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

a <- Sys.time()

#cl <- parallel::makeCluster(n_cores-2)
cl <- parallel::makeCluster(1)
doParallel::registerDoParallel(cl)

simres <- foreach(i=1:nrow(conditions),
        .combine = rbind,
        .packages = "fitPCurve") %dopar% {

#for (i in 1:nrow(conditions)) {
  ps <- sim.multDVhack (nvar=conditions[i,1],
                        r=conditions[i,2],
                        d=conditions[i,3],
                        iter = conditions[i,4],
                        alpha = conditions[i,5]
                        )

   res_pcurve <- matrix(c(
    compute_pcurve(ps[,1]),
    compute_pcurve(ps[,2]),
    compute_pcurve(ps[,3])),
    byrow=TRUE,
    ncol=5
  )

  res_i <- cbind(
    # The conditions; adding the selection method 1, 2, and 3
    cbind(
      matrix(rep(conditions[i, ], each = 3), nrow = 3, byrow = FALSE),
      1:3),
    res_pcurve
  )
  res_i
}

parallel::stopCluster(cl)
b <- Sys.time()-a
b

colnames(simres) <- c(
  colnames(conditions),
  "strategy",
  paste0("p", 1:5)
)

write.csv(simres, "../simulations/sim-results/worstCase.csv")
