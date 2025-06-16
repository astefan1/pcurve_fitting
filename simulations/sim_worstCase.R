# ==============================================================================
# Worst Case Scenario: No effect, only p-hacking (selective reporting of DV)
# ==============================================================================

library(foreach)

nvar <- seq(2, 200, by = 2)
r <- seq(0, 0.9, by = 0.1)
d <- 0
strategy <- c("firstsig", "smallest", "smallest.sig")
iter <- 10000
alternative <- "two.sided"
alpha <- 0.05

simres <- expand.grid(nvar, r, d, strategy, iter, alternative, alpha, stringsAsFactors = FALSE)
colnames(simres) <- c("nvar", "r", "d", "strategy", "iter", "alternative", "alpha")
simres[, c(8:12)] <- NA
colnames(simres[, c(8:12)]) <- paste0("p", c(1:5))

a <- Sys.time()

cl <- parallel::makeCluster(4)
doParallel::registerDoParallel(cl)

foreach(i=1:nrow(simres)) %dopar% {
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

write.csv(simres, "./sim-results/worstCase.csv")
