# ==============================================================================
# Worst Case Scenario: No effect, only p-hacking (selective reporting of DV)
# ==============================================================================

library(fitPCurve)
library(rio)
library(dplyr)

pcurves <- import("../simulations/pcurves-to-fit.csv")
#simres <- import("../simulations/sim-results/sim_worstCase.csv")
simres <- import("../simulations/sim-results/sim_A.csv")

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

# TODO: add labels to facets; 1: smallest, 2: smallest significant, 3: first significant as labels

library(patchwork)
p1 <- p_heatmap(simres, "rmseSotola")
p2 <- p_heatmap(simres, "rmseWetzels")
p3 <- p_heatmap(simres, "rmseSimonsohn")
p1 / p2 / p3



# TODO: Check that no averaging happens!

ggplot(simres |> filter(nvar==2, prop_Hacker == 0.2), aes(x = d, y = prop_H1, fill = rmseWetzels)) +
  geom_tile() +
  facet_grid(het~r) +
  scale_fill_viridis_c(
    option = "inferno",
    direction = -1,
    values = c(0, 0.1, 1),
    limits=c(
      min(simres |> select(contains("rmse"))),
      max(simres |> select(contains("rmse")))
    )
  ) +
  scale_y_continuous(breaks = sort(unique(simres$r)))



# Get variable importance:
library(ranger)
r1 <- ranger(rmseSotola ~ nvar + r + d + prop_Hacker + prop_H1 + het, data=simres, importance='permutation')
importance(r1)



## Test the bump with a custom p-curve

plot_pcurves(
  simres |> filter(strategy==1, prop_Hacker==0.2, d==0.1, nvar==50),
  poriginal = c(20, 20, 20, 20, 20),
  n_best = 1000)
