library(doParallel)
library(doRNG)  # Required for %dorng%
library(foreach)
library(rio)
source("../simulations/sim_function.R")

# A (prop_H1 > 0 and prop_Hacker > 0): 17 (nvar) x 4 (r) x 10 (d) x 6 (het) x 5 (prop_phacker) x 10 (prop_H1) = 204000
# B (prop_H1 = 0 and prop_Hacker = 0): 1 (prop_phacker) x 1 (prop_H1) = 1
# C (prop_H1 > 0 and prop_Hacker = 0): 10 (d) x 6 (het) x 1 (prop_phacker) x 10 (prop_H1) = 600
# D (prop_H1 = 0 and prop_Hacker > 0): 17 (nvar) x 4 (r) x 5 (prop_phacker) x 10 (prop_H1) = 3400


# =============================================================================
# Realistic condition (A)
# =============================================================================

# Set seed for reproducibility
set.seed(12345)

# nvar applies to p-hacking researchers; non-hackers have nvar=1
# note: this approximates a logarithmic progression of nvar:
# floor(exp(seq(log(2), log(200), by = 0.2))) |> unique()

nvar = c(2, 3, 5, 8, floor(exp(seq(log(10), log(150), by = 0.3))) |> unique())
r = seq(0, 0.9, by = 0.3)
d = seq(0.1, 0.8, by = 0.1) # ES under H1; under H0 it's always 0
het = seq(0, 0.4, by = 0.1)
prop_Hacker = c(0.1, seq(0.2, 1, by = 0.2)) # proportion of researchers practicing p-hacking
prop_H1 = seq(0.1, .7, by = 0.1) # proportion of true H1 effects

iter = 10000   # this is fixed
alpha = 0.05 # this is fixed

# Create grid of conditions
realistic_conditions <- expand.grid(nvar, r, d, prop_Hacker, prop_H1, het, iter, alpha, stringsAsFactors = FALSE)
colnames(realistic_conditions) <- c("nvar", "r", "d", "prop_Hacker", "prop_H1", "het", "iter", "alpha")

cat("Total conditions to process:", nrow(realistic_conditions), "\n")

simres <- sim_pcurve("sim_realistic", realistic_conditions, n_cores = 10)

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
het = seq(0, 0.4, by = 0.1)
prop_Hacker = 0
prop_H1 = seq(0.1, .5, by = 0.1) # proportion of true H1 effects

iter = 10000   # this is fixed
alpha = 0.05 # this is fixed

# Create grid of conditions
perfect_conditions <- expand.grid(nvar, r, d, prop_Hacker, prop_H1, het, iter, alpha, stringsAsFactors = FALSE)
colnames(perfect_conditions) <- c("nvar", "r", "d", "prop_Hacker", "prop_H1", "het", "iter", "alpha")

cat("Total conditions to process:", nrow(perfect_conditions), "\n")

simres <- sim_pcurve("sim_perfect", perfect_conditions, n_cores = 10)

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

nvar = c(2, 3, 5, 8, floor(exp(seq(log(10), log(150), by = 0.3))) |> unique())
r = seq(0, 0.9, by = 0.3)
d = 0
het = 0
prop_Hacker = c(0.1, seq(0.2, 1, by = 0.2)) # proportion of researchers practicing p-hacking
prop_H1 = 0

iter = 10000   # this is fixed
alpha = 0.05 # this is fixed

# Create grid of conditions
worst_conditions <- expand.grid(nvar, r, d, prop_Hacker, prop_H1, het, iter, alpha, stringsAsFactors = FALSE)
colnames(worst_conditions) <- c("nvar", "r", "d", "prop_Hacker", "prop_H1", "het", "iter", "alpha")

cat("Total conditions to process:", nrow(worst_conditions), "\n")

simres <- sim_pcurve("sim_worst", worst_conditions, n_cores = 10)

# Save results
export(simres, paste0("../simulations/sim-results/sim_worst.csv"))












#---------------------------------------------------
# Optionally: Aggregate intermediate files
# (e.g., if the computation has been interrupted due to an error)


simres <- collect_rds("/Users/felix/Documents/Github/pcurve_fitting/simulations/sim-results/sim_realistic")
export(simres, paste0("../simulations/sim-results/sim_realistic.csv"))

library(data.table)

im_files <- list.files(paste0("../simulations/sim-results/", sim_name), pattern=paste0(sim_name,"_\\d*.csv"), full.names = TRUE)

combined_data <- rbindlist(lapply(im_files, fread))
combined_data$V1 <- NULL
result_matrix <- as.matrix(combined_data)

export(result_matrix, paste0("../simulations/sim-results/", sim_name, ".csv"))
