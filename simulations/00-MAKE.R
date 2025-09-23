# This scripts runs all simulations and analyses in a reproducible manner.
# For exact reproducibility, you need at least 6 cores and 16GB of RAM.

# Set seed for reproducibility
set.seed(12345)

# create a file with three empirical reference p-curves
source("reference-pcurves.R")

# Run simulations for the "multiple DVs" p-hacking strategy
source("sim_multDV_scenarios.R")

# Run simulations for the "optional stopping" p-hacking strategy
source("sim_optionalStopping.R")