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
simres <- import("../simulations/sim-results/sim_worst.csv")
#simres <- import("../simulations/sim-results/sim_perfect.csv")
#simres_H0 <- import("../simulations/sim-results/sim_H0.csv")

simres$rmseSotola    <- apply(simres[, paste0("p", 1:5)], 1, function(x) rmse(x, simplify2array(pcurves[1, paste0("p", 1:5)])))
simres$rmseWetzels   <- apply(simres[, paste0("p", 1:5)], 1, function(x) rmse(x, simplify2array(pcurves[2, paste0("p", 1:5)])))
simres$rmseSimonsohn <- apply(simres[, paste0("p", 1:5)], 1, function(x) rmse(x, simplify2array(pcurves[3, paste0("p", 1:5)])))

simres$chi2Sotola    <- apply(simres[, paste0("p", 1:5)], 1, function(x)
    chi2(x, simplify2array(pcurves[1, paste0("p", 1:5)]), n = pcurves$nsig[1])
  )
simres$chi2Wetzels   <- apply(simres[, paste0("p", 1:5)], 1, function(x)
    chi2(x, simplify2array(pcurves[2, paste0("p", 1:5)]), n = pcurves$nsig[2])
  )
simres$chi2Simonsohn <- apply(simres[, paste0("p", 1:5)], 1, function(x)
    chi2(x, simplify2array(pcurves[3, paste0("p", 1:5)]), n = pcurves$nsig[3])
  )

cor(simres$rmseSotola, simres$chi2Sotola)
cor(simres$rmseWetzels, simres$chi2Wetzels)
cor(simres$rmseSimonsohn, simres$chi2Simonsohn)

simres[order(simres$rmseSotola)[1:10], ]
simres[order(simres$rmseWetzels)[1:10], ]
simres[order(simres$rmseSimonsohn)[1:10], ]

plot(simres$rmseSotola[order(simres$rmseSotola)], ylab="RMSE")
plot(simres$rmseWetzels[order(simres$rmseWetzels)], ylab="RMSE")
plot(simres$rmseSimonsohn[order(simres$rmseSimonsohn)], ylab="RMSE")

plot_pcurves(simdat = simres, poriginal = pcurves[pcurves$dataset == "sotola", paste0("p", 1:5)], n_best=10)
plot_pcurves(simdat = simres, poriginal = pcurves[pcurves$dataset == "wetzels", paste0("p", 1:5)], n_best=10)
plot_pcurves(simdat = simres, poriginal = pcurves[pcurves$dataset == "simonsohn", paste0("p", 1:5)], n_best=10)


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

# realistic scenario:
simres$rmseBump <- apply(simres[, paste0("p", 1:5)], 1, function(x) rmse(x, c(15, 15, 15, 15, 40)))
plot_pcurves(simres, poriginal = c(15, 15, 15, 15, 40), n_best = 10)
simres |> arrange(rmseBump) |> slice(1:5)

# H0 scenario
simres_H0$rmseBump <- apply(simres_H0[, paste0("p", 1:5)], 1, function(x) rmse(x, c(15, 15, 15, 15, 40)))
plot_pcurves(simres_H0, poriginal = c(15, 15, 15, 15, 40), n_best = 3)
simres_H0 |> arrange(rmseBump)
