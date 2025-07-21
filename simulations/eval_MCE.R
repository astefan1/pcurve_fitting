# ==============================================================================
# Eval scenarios
# ==============================================================================

library(fitPCurve)
library(rio)
library(dplyr)
library(ggplot2)

# reference p-curves
pcurves <- import("../simulations/pcurves-to-fit.csv")

simres_1000 <- import("../simulations/sim-results/sim_test_MCSE_1000.csv")
plot_pcurves(simres_1000, poriginal = pcurves$simonsohn, alpha=.3)

simres_10000 <- import("../simulations/sim-results/sim_test_MCSE_10000.csv")
plot_pcurves(simres_10000, poriginal = pcurves$simonsohn, alpha=.3)
