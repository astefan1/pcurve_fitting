library(doParallel)
library(doRNG)  # Required for %dorng%
library(foreach)
library(rio)

# Set seed for reproducibility
set.seed(12345)
sim_name <- "sim_complex"

#---------------------------------------------------
# Final simulation conditions

# nvar applies to p-hacking researchers; non-hackers have nvar=1
# note: this approximates a logarithmic progression of nvar:
# floor(exp(seq(log(2), log(200), by = 0.2))) |> unique()

nvar = c(2:8, floor(exp(seq(log(10), log(150), by = 0.3))) |> unique())
r = seq(0, 0.9, by = 0.3)
d = seq(0.1, 1, by = 0.1) # ES under H1; under H0 it's always 0
het = seq(0, 0.5, by = 0.1)
iter = 10000 # this is fixed
alpha = 0.05 # this is fixed
prop_phacker = seq(0, 1, by = 0.2) # proportion of researchers practicing p-hacking, 0 = best case, 1 = worst case
prop_H1 = seq(0, 1, by = 0.1) # proportion of true H1 effects


#---------------------------------------------------
# NOTHING NEEDS TO BE CHANGED FROM HERE ON -->
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
dir.create(paste0("../simulations/sim-results/", sim_name))
start_time <- Sys.time()

# Run parallel simulation with reproducible RNG
simres <- foreach(i = 1:nrow(conditions),
                  .combine = rbind,
                  .packages = "fitPCurve") %dorng% {  # Using %dorng% instead of %dopar%

                      ps <- tryCatch({
                        sim.multDVhack(nvar = conditions[i, 1],
                                           r = conditions[i, 2],
                                           d = conditions[i, 3],
                                           het = conditions[i, 4],
                                           iter = conditions[i, 5],
                                           alpha = conditions[i, 6])
                        }, error = function(e) return(matrix(NA, nrow=iter, ncol=3))
                        )

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
                    write(progress_msg, file = paste0("../simulations/sim-results/", sim_name, "/", sim_name,"_progress.txt"), append = TRUE)

                    # write intermediate file
                    export(res_i, paste0("../simulations/sim-results/", sim_name, "/", sim_name,"_", i, ".csv"))
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

# Save results
export(result_matrix, paste0("../simulations/sim-results/", sim_name, ".csv"))


#---------------------------------------------------
# Optionally: Aggregate intermediate files
# (e.g., if the computation has been interrupted due to an error)

library(data.table)

im_files <- list.files(paste0("../simulations/sim-results/", sim_name), pattern=paste0(sim_name,"_\\d*.csv"), full.names = TRUE)

combined_data <- rbindlist(lapply(im_files, fread))
combined_data$V1 <- NULL
result_matrix <- as.matrix(combined_data)

export(result_matrix, paste0("../simulations/sim-results/", sim_name, ".csv"))
