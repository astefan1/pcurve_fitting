# ==============================================================================
# Worst Case Scenario: No effect, only p-hacking (selective reporting of DV)
# ==============================================================================

nobs.group <- 100
nvar <- seq(2, 20, by = 2)
r <- seq(0, 0.9, by = 0.3)
d <- 0
strategy <- c("firstsig", "smallest", "smallest.sig")
iter <- 1000
alternative <- "two.sided"
alpha <- 0.05


simres <- expand.grid(nobs.group, nvar, r, d, strategy, iter, alternative, alpha, stringsAsFactors = FALSE)
colnames(simres) <- c("nobs.group", "nvar", "r", "d", "strategy", "iter", "alternative", "alpha")
simres[, c(9:13)] <- NA
colnames(simres[, c(9:13)]) <- paste0("p", c(1:5))

a <- Sys.time()
for(i in 1:nrow(simres)){
  ps <- sim.multDVhack (nobs.group=simres[i,1],
                        nvar=simres[i,2],
                        r=simres[i,3],
                        d=simres[i,4],
                        strategy = simres[i,5],
                        iter = simres[i,6],
                        alternative = simres[i,7],
                        alpha = simres[i,8]
                        )
  simres[i,9:13] <- compute_pcurve(ps)

}
b <- Sys.time()-a
b

write.csv(simres, "../simulations/sim-results/worstCase.csv")
