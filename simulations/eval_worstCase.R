# ==============================================================================
# Worst Case Scenario: No effect, only p-hacking (selective reporting of DV)
# ==============================================================================

pcurves <- read.csv("../simulations/pcurves-to-fit.csv")
simres <- read.csv("../simulations/sim-results/worstCase.csv")

simres$rmseSotola <- apply(simres[, 10:14], 1, function(x) rmse(x, pcurves$sotola))
simres$rmseWetzels <- apply(simres[, 10:14], 1, function(x) rmse(x, pcurves$wetzels))
simres$rmseSimonsohn <- apply(simres[, 10:14], 1, function(x) rmse(x, pcurves$simonsohn))

simres[order(simres$rmseSotola)[1:5], ]
simres[order(simres$rmseWetzels)[1:5], ]
simres[order(simres$rmseSimonsohn)[1:5], ]

plot(simres$rmseSotola[order(simres$rmseSotola)])
plot(simres$rmseWetzels[order(simres$rmseWetzels)])
plot(simres$rmseSimonsohn[order(simres$rmseSimonsohn)])

plot_pcurves(simres, poriginal = pcurves$sotola)
plot_pcurves(simres, poriginal = pcurves$wetzels)
plot_pcurves(simres, poriginal = pcurves$simonsohn)

## Heatmap

library(patchwork)
p1 <- heatmap(simres, "rmseSotola")
p2 <- heatmap(simres, "rmseWetzels")
p3 <- heatmap(simres, "rmseSimonsohn")
p1 / p2 / p3
