# ==============================================================================
# Eval scenarios
# ==============================================================================

library(fitPCurve)
library(rio)
library(dplyr)
library(ggplot2)

# reference p-curves
pcurves <- import("../simulations/pcurves-to-fit.csv")

# load the simulation results here:
#simres <- import("../simulations/sim-results/sim_realistic.csv")
simres <- import("../simulations/sim-results/sim_realistic_1000.csv")
#simres <- import("../simulations/sim-results/sim_worst.csv")
#simres <- import("../simulations/sim-results/sim_perfect.csv")
#simres <- import("../simulations/sim-results/sim_H0.csv")

simres$rmseSotola    <- apply(simres[, paste0("p", 1:5)], 1, function(x) rmse(x, pcurves$sotola))
simres$rmseWetzels   <- apply(simres[, paste0("p", 1:5)], 1, function(x) rmse(x, pcurves$wetzels))
simres$rmseSimonsohn <- apply(simres[, paste0("p", 1:5)], 1, function(x) rmse(x, pcurves$simonsohn))

simres[order(simres$rmseSotola)[1:10], ]
simres[order(simres$rmseWetzels)[1:10], ]
simres[order(simres$rmseSimonsohn)[1:10], ]

plot(simres$rmseSotola[order(simres$rmseSotola)], ylab="RMSE")
plot(simres$rmseWetzels[order(simres$rmseWetzels)], ylab="RMSE")
plot(simres$rmseSimonsohn[order(simres$rmseSimonsohn)], ylab="RMSE")

plot_pcurves(simres, poriginal = pcurves$sotola, n_best=10)
plot_pcurves(simres, poriginal = pcurves$wetzels, n_best=10)
plot_pcurves(simres, poriginal = pcurves$simonsohn, n_best=10)


# check a single result:
simres |> filter(nvar == 2, r == 0, d == 0.3, prop_Hacker == 0.8, prop_H1 == 0.4, het == 0, strategy == 3)

# A heatmap with all conditions (no averaging; each condition is one tile)
ggplot(simres, aes(x = d, y = prop_H1, fill = rmseSimonsohn)) +
  geom_tile() +
  facet_grid(
    rows = vars(het, nvar, strategy),      # two stacked facet rows
    cols = vars(r, prop_Hacker)  # two stacked facet columns
  ) +
  scale_fill_viridis_c(
    option = "inferno",
    direction = -1,
    values = c(0, 0.1, 1),
    limits=c(
      min(simres |> select(contains("rmse"))),
      max(simres |> select(contains("rmse")))
    )
  ) +
  scale_y_continuous(breaks = sort(unique(simres$r))) +
  theme_minimal(base_size = 9) +
  theme(
    panel.spacing.x = unit(0.01, "lines"),
    panel.spacing.y = unit(0.01, "lines")
  )







## Test the bump with a custom p-curve

plot_pcurves(
  #simres |> filter(strategy==1, prop_Hacker==0.2, d==0.1, nvar==50),
  simres,
  poriginal = c(15, 15, 15, 15, 40),
  n_best = 10)
