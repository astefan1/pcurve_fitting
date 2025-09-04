library(ranger)
library(partykit)
library(iml)

simres <- import("simulations/sim-results/sim_worst.csv")

# Get variable importance:
rf_dat <- simres |>
  filter(!is.na(rmseSimonsohn)) |>
  select(rmseSimonsohn, nvar, r, d, prop_Hacker, prop_H1, het, strategy)

X  <- rf_dat |> select(-rmseSimonsohn)

rf <- ranger(rmseSimonsohn ~ nvar + r + d + prop_Hacker + prop_H1 + het + strategy, data=rf_dat, importance='permutation')
importance(rf)


# Surrogate model: Fit a binary tree to the results
X$pred <- rf$predictions
c1 <- ctree(pred~., data=X, control=ctree_control(maxdepth=4))
plot(c1)



# interpretable machine learning
# not sure if the marginal effects are very informative,
# as we see massive higher-order interactions

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
