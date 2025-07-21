library(doParallel)
library(doRNG)  # Required for %dorng%
library(foreach)
library(rio)
source("../simulations/sim_function.R")

# test the top Simonsohn condition
nvar = 2
r = 0
d = 0.3
het = 0
prop_Hacker = 0.8
prop_H1 = 0.4
alpha = 0.05 # this is fixed

test_condition0 <- expand.grid(nvar, r, d, prop_Hacker, prop_H1, het, iter, alpha, stringsAsFactors = FALSE)
colnames(test_condition0) <- c("nvar", "r", "d", "prop_Hacker", "prop_H1", "het", "iter", "alpha")


# =============================================================================
# 1000 iterations (repeated 1000 times)
# =============================================================================

# Set seed for reproducibility
set.seed(12345)
iter = 1000   # this is fixed
meta_iter = 1000 # Monte Carlo repetitions

# Create grid of conditions
test_conditions <- test_condition0 %>% slice(rep(1, each = meta_iter))

cat("Total conditions to process:", nrow(test_conditions), "\n")

simres <- sim_pcurve("sim_test_MCSE", test_conditions, n_cores = 10)

# Save results
export(simres, paste0("../simulations/sim-results/sim_test_MCSE_1000.csv"))

# =============================================================================
# 10,000 iterations (repeated 1000 times)
# =============================================================================

# Set seed for reproducibility
set.seed(12345)
iter = 1000   # this is fixed
meta_iter = 10000 # Monte Carlo repetitions

# Create grid of conditions
test_conditions <- test_condition0 %>% slice(rep(1, each = meta_iter))

cat("Total conditions to process:", nrow(test_conditions), "\n")

simres <- sim_pcurve("sim_test_MCSE", test_conditions, n_cores = 10)

# Save results
export(simres, paste0("../simulations/sim-results/sim_test_MCSE_10000.csv"))
