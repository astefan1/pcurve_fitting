# ==============================================================================
# Worst Case Scenario: No effect, only p-hacking (selective reporting of DV)
# ==============================================================================

library(rio)

pcurves <- import("../simulations/pcurves-to-fit.csv")
simres <- import("../simulations/sim-results/sim_worstCase.csv")

simres$rmseSotola    <- apply(simres[, paste0("p", 1:5)], 1, function(x) rmse(x, pcurves$sotola))
simres$rmseWetzels   <- apply(simres[, paste0("p", 1:5)], 1, function(x) rmse(x, pcurves$wetzels))
simres$rmseSimonsohn <- apply(simres[, paste0("p", 1:5)], 1, function(x) rmse(x, pcurves$simonsohn))

simres[order(simres$rmseSotola)[1:5], ]
simres[order(simres$rmseWetzels)[1:5], ]
simres[order(simres$rmseSimonsohn)[1:5], ]

plot(simres$rmseSotola[order(simres$rmseSotola)], ylab="RMSE")
plot(simres$rmseWetzels[order(simres$rmseWetzels)], ylab="RMSE")
plot(simres$rmseSimonsohn[order(simres$rmseSimonsohn)], ylab="RMSE")

plot_pcurves(simres, poriginal = pcurves$sotola)
plot_pcurves(simres, poriginal = pcurves$wetzels)
plot_pcurves(simres, poriginal = pcurves$simonsohn)

## Heatmap

library(patchwork)
p1 <- heatmap(simres, "rmseSotola")
p2 <- heatmap(simres, "rmseWetzels")
p3 <- heatmap(simres, "rmseSimonsohn")
p1 / p2 / p3
