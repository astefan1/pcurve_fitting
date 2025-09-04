library(doParallel)
library(doRNG)  # Required for %dorng%
library(foreach)
library(rio)
source("../simulations/sim_function.R")

# A (prop_H1 > 0 and prop_Hacker > 0): 6 (nvar) x 4 (r) x 8 (d) x 3 (het) x 5 (prop_phacker) x 5 (prop_H1) = 14400
# B (prop_H1 = 0 and prop_Hacker = 0): 1 (prop_phacker) x 1 (prop_H1) = 1
# C (prop_H1 > 0 and prop_Hacker = 0): 8 (d) x 3 (het) x 1 (prop_phacker) x 5 (prop_H1) = 120
# D (prop_H1 = 0 and prop_Hacker > 0): 6 (nvar) x 4 (r) x 5 (prop_phacker) x 5 (prop_H1) = 600


# =============================================================================
# Realistic condition (A)
# =============================================================================

# Set seed for reproducibility
set.seed(12345)

# nvar applies to p-hacking researchers; non-hackers have nvar=1
# note: this approximates a logarithmic progression of nvar:
# floor(exp(seq(log(2), log(200), by = 0.2))) |> unique()

nvar = c(2, 5, 10, 30, 70, 150) # inspired by floor(exp(seq(log(10), log(150), by = 0.3))) |> unique())
r = seq(0, 0.9, by = 0.3)
d = seq(0.1, 0.8, by = 0.1) # ES under H1; under H0 it's always 0
het = seq(0, 0.4, by = 0.2)
prop_Hacker = c(0.1, seq(0.25, 1, by = 0.25)) # proportion of researchers practicing p-hacking
prop_H1 = c(0.1, 0.2, 0.3, 0.5, 0.7) # proportion of true H1 effects

iter = 10000   # this is fixed
alpha = 0.05 # this is fixed

# Create grid of conditions
realistic_conditions <- expand.grid(nvar, r, d, prop_Hacker, prop_H1, het, iter, alpha, stringsAsFactors = FALSE)
colnames(realistic_conditions) <- c("nvar", "r", "d", "prop_Hacker", "prop_H1", "het", "iter", "alpha")

cat("Total conditions to process:", nrow(realistic_conditions), "\n")

simres <- sim_pcurve("sim_realistic", realistic_conditions, n_cores = 6)

# Save results
export(simres, paste0("../simulations/sim-results/sim_realistic.csv"))


# =============================================================================
# Baseline condition (B)
# =============================================================================

# Set seed for reproducibility
set.seed(12345)

nvar = 1
r = 0
d = 0
het = 0
prop_Hacker = 0
prop_H1 = 0

iter = 10000   # this is fixed
alpha = 0.05 # this is fixed

# Create grid of conditions
H0_conditions <- expand.grid(nvar, r, d, prop_Hacker, prop_H1, het, iter, alpha, stringsAsFactors = FALSE)
colnames(H0_conditions) <- c("nvar", "r", "d", "prop_Hacker", "prop_H1", "het", "iter", "alpha")

cat("Total conditions to process:", nrow(H0_conditions), "\n")

simres <- sim_pcurve("sim_H0", H0_conditions, n_cores = 1)

# Save results
export(simres, paste0("../simulations/sim-results/sim_H0.csv"))


# =============================================================================
# Perfect case condition (C)
# =============================================================================

# Set seed for reproducibility
set.seed(12345)

# nvar applies to p-hacking researchers; non-hackers have nvar=1
# note: this approximates a logarithmic progression of nvar:
# floor(exp(seq(log(2), log(200), by = 0.2))) |> unique()

nvar = 1
r = 0
d = seq(0.1, 0.8, by = 0.1) # ES under H1; under H0 it's always 0
het = seq(0, 0.4, by = 0.2)
prop_Hacker = 0
prop_H1 = c(0.1, 0.2, 0.3, 0.5, 0.7) # proportion of true H1 effects

iter = 10000   # this is fixed
alpha = 0.05 # this is fixed

# Create grid of conditions
perfect_conditions <- expand.grid(nvar, r, d, prop_Hacker, prop_H1, het, iter, alpha, stringsAsFactors = FALSE)
colnames(perfect_conditions) <- c("nvar", "r", "d", "prop_Hacker", "prop_H1", "het", "iter", "alpha")

cat("Total conditions to process:", nrow(perfect_conditions), "\n")

simres <- sim_pcurve("sim_perfect", perfect_conditions, n_cores = 8)

# Save results
export(simres, paste0("../simulations/sim-results/sim_perfect.csv"))






# =============================================================================
# Worst case condition (D)
# =============================================================================

# Set seed for reproducibility
set.seed(12345)

# nvar applies to p-hacking researchers; non-hackers have nvar=1
# note: this approximates a logarithmic progression of nvar:
# floor(exp(seq(log(2), log(200), by = 0.2))) |> unique()

nvar = c(2, 5, 10, 30, 70, 150) # inspired by floor(exp(seq(log(10), log(150), by = 0.3))) |> unique())
r = seq(0, 0.9, by = 0.3)
d = 0
het = 0
prop_Hacker = c(0.1, seq(0.25, 1, by = 0.25)) # proportion of researchers practicing p-hacking
prop_H1 = 0

iter = 10000   # this is fixed
alpha = 0.05 # this is fixed

# Create grid of conditions
worst_conditions <- expand.grid(nvar, r, d, prop_Hacker, prop_H1, het, iter, alpha, stringsAsFactors = FALSE)
colnames(worst_conditions) <- c("nvar", "r", "d", "prop_Hacker", "prop_H1", "het", "iter", "alpha")

cat("Total conditions to process:", nrow(worst_conditions), "\n")

simres <- sim_pcurve("sim_worst", worst_conditions, n_cores = 8)

# Save results
export(simres, paste0("../simulations/sim-results/sim_worst.csv"))

stop();










#---------------------------------------------------
# Optionally: Aggregate intermediate files
# (e.g., if the computation has been interrupted due to an error)


simres <- collect_rds(root="/Users/felix/Documents/Github/pcurve_fitting/simulations/sim-results/sim_realistic")
export(simres, paste0("../simulations/sim-results/sim_realistic.csv"))
