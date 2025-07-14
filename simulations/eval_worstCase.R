# ==============================================================================
# Worst Case Scenario: No effect, only p-hacking (selective reporting of DV)
# ==============================================================================

library(fitPCurve)
library(rio)
library(dplyr)
library(ggplot2)

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

plot_pcurves(simres, poriginal = pcurves$sotola, n_best=10)
plot_pcurves(simres, poriginal = pcurves$wetzels, n_best=10)
plot_pcurves(simres, poriginal = pcurves$simonsohn, n_best=10)

## Heatmap

# TODO: add labels to facets; 1: smallest, 2: smallest significant, 3: first significant as labels

library(patchwork)
p1 <- p_heatmap(simres, "rmseSotola")
p2 <- p_heatmap(simres, "rmseWetzels")
p3 <- p_heatmap(simres, "rmseSimonsohn")
p1 / p2 / p3



# TODO: Check that no averaging happens!

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




library(GGally)

GGally::ggparcoord(
  simres,
  columns = c("nvar", "r", "d", "prop_Hacker",
              "prop_H1", "het", "strategy", "rmseSimonsohn"),
  scale = "uniminmax",
  groupColumn = "strategy",           # colour by strategy
  alphaLines = 0.2
) +
  scale_colour_viridis_d(option = "H") +
  labs(title = "All design factors vs. RMSE")



# Get variable importance:
library(ranger)
rf_dat <- simres |>
  filter(!is.na(rmseSimonsohn)) |>
  select(rmseSimonsohn, nvar, r, d, prop_Hacker, prop_H1, het, strategy)

X  <- rf_dat |> select(-rmseSimonsohn)

rf <- ranger(rmseSimonsohn ~ nvar + r + d + prop_Hacker + prop_H1 + het + strategy, data=rf_dat, importance='permutation')
importance(rf)


# Surrogate model: Fit a binary tree to the results
library(partykit)

X$pred <- rf$predictions
c1 <- ctree(pred~., data=X, control=ctree_control(maxdepth=3))
plot(c1)






library(iml)


mod <- Predictor$new(
  model   = rf,
  data    = X,
  y       = rf_dat$rmseSimonsohn,
  predict.function = function(m, newdata) {
    predict(m, data = newdata)$predictions
  }
)

## ALE plots
FeatureEffect$new(mod, feature = "strategy", method = "ale") |> plot()
FeatureEffect$new(mod, feature = "nvar", method = "ale") |> plot()
FeatureEffect$new(mod, feature = "d", method = "ale") |> plot()
FeatureEffect$new(mod, feature = "r", method = "ale") |> plot()
FeatureEffect$new(mod, feature = "het", method = "ale") |> plot()
FeatureEffect$new(mod, feature = "prop_Hacker", method = "ale") |> plot()
FeatureEffect$new(mod, feature = "prop_H1", method = "ale") |> plot()



# Reduced heatmap
ggplot(simres |> filter(strategy == 1, het==0), aes(x = d, y = prop_H1, fill = rmseSimonsohn)) +
  geom_tile() +
  facet_grid(
    rows = vars(nvar),      # two stacked facet rows
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
  simres |> filter(strategy==1, prop_Hacker==0.2, d==0.1, nvar==50),
  poriginal = c(20, 20, 20, 20, 20),
  n_best = NA)
