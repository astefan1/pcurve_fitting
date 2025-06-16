# ==============================================================================
# Worst Case Scenario: No effect, only p-hacking (selective reporting of DV)
# ==============================================================================

nvar <- seq(2, 200, by = 2)
r <- seq(0, 0.9, by = 0.1)
d <- 0
iter <- 10000
alpha <- 0.05

conditions <- expand.grid(nvar, r, d, iter, alpha, stringsAsFactors = FALSE)
colnames(conditions) <- c("nvar", "r", "d", "iter", "alpha")

simresArray <- array(data = NA, dim=c(nrow(conditions), 5, 3))

a <- Sys.time()
for(i in 1:nrow(conditions)){
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
b <- Sys.time()-a
b
simresDF <- rbind(cbind(conditions, strategy="smallest", simresArray[,,1]),
                  cbind(conditions, strategy="smallest.sig", simresArray[,,2]),
                  cbind(conditions, strategy="firstsig", simresArray[,,3]))
colnames(simresDF)[7:11] <- paste0("p", 1:5)

write.csv(simresDF, "../simulations/sim-results/worstCase.csv")
