# ==============================================================================
# Best Case Scenario: No effect, only p-hacking (selective reporting of DV)
# ==============================================================================

library(doParallel)
library(doRNG)  # Required for %dorng%
library(foreach)

# Set seed for reproducibility
set.seed(12345)
progress_file <- "../simulations/sim-results/sim_best_case_progress.txt"

#---------------------------------------------------
# Final simulation conditions
nvar <- seq(2, 200, by = 2)
r <- seq(0, 0.9, by = 0.1)
d <- seq(0.1, 1, by = 0.1)
het <- seq(0, 0.5, by = 0.05)
iter <- 10000
alpha <- 0.05

# Do a limited simulation for testing
# nvar <- c(2, 5, 10)
# r <- c(0, 0.5)
# d <- c(.1, .2, .5)
# het <- c(0, .2, .4)
# iter <- 1000
# alpha <- 0.05

#---------------------------------------------------
# Prepare the parallel processing

# Clean up any existing parallel backends
try(stopCluster(cl), silent = TRUE)
stopImplicitCluster()
registerDoSEQ()  # Reset to sequential

# How many cores does your CPU have?
n_cores <- detectCores()
cat("Available cores:", n_cores, "\n")

# Setup parallel cluster
cl <- makeCluster(n_cores - 2)
registerDoParallel(cl)

#---------------------------------------------------

# Create grid of conditions
conditions <- expand.grid(nvar, r, d, het, iter, alpha, stringsAsFactors = FALSE)
colnames(conditions) <- c("nvar", "r", "d", "het", "iter", "alpha")

cat("Total conditions to process:", nrow(conditions), "\n")


# Progress tracking setup
cat("Processing", nrow(conditions), "conditions...\n")
cat("Progress file:", progress_file, "\n")
start_time <- Sys.time()

# Run parallel simulation with reproducible RNG
simres <- foreach(i = 1:nrow(conditions),
                  .combine = rbind,
                  .packages = "fitPCurve") %dorng% {  # Using %dorng% instead of %dopar%

                    # Your simulation code
                    ps <- sim.multDVhack(nvar = conditions[i, 1],
                                         r = conditions[i, 2],
                                         d = conditions[i, 3],
                                         het = conditions[i, 4],
                                         iter = conditions[i, 5],
                                         alpha = conditions[i, 6])

                    res_pcurve <- matrix(c(
                      compute_pcurve(ps[, 1]),
                      compute_pcurve(ps[, 2]),
                      compute_pcurve(ps[, 3])),
                      byrow = TRUE,
                      ncol = 5
                    )

                    res_i <- cbind(
                      # The conditions; adding the selection method 1, 2, and 3
                      cbind(
                        matrix(rep(conditions[i, ], each = 3), nrow = 3, byrow = FALSE),
                        1:3),
                      res_pcurve
                    )

                    colnames(res_i) <- c(
                      colnames(conditions),
                      "strategy",
                      paste0("p", 1:5)
                    )

                    # Simple progress tracking in a file
                    progress_msg <- sprintf("Completed %d/%d (%.1f%%) at %s",
                                            i, nrow(conditions), 100 * i / nrow(conditions), Sys.time())
                    write(progress_msg, file = progress_file, append = TRUE)

                    res_i
                  }

# Calculate total time
total_time <- difftime(Sys.time(), start_time, units = "mins")
cat("Total processing time:", round(total_time, 2), "minutes\n")

# Clean shutdown
stopCluster(cl)

# Display results summary
cat("First few rows:\n")
print(head(simres))

# Optional: Save results
write.csv(simres, "../simulations/sim-results/bestCase.csv")
