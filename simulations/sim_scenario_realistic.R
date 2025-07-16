library(doParallel)
library(doRNG)  # Required for %dorng%
library(foreach)
library(rio)

# A (prop_H1 > 0 and prop_Hacker > 0): 17 (nvar) x 4 (r) x 10 (d) x 6 (het) x 5 (prop_phacker) x 10 (prop_H1) = 204000
# B (prop_H1 = 0 and prop_Hacker = 0): 1 (prop_phacker) x 1 (prop_H1) = 1
# C (prop_H1 > 0 and prop_Hacker = 0): 10 (d) x 6 (het) x 1 (prop_phacker) x 10 (prop_H1) = 600
# D (prop_H1 = 0 and prop_Hacker > 0): 17 (nvar) x 4 (r) x 5 (prop_phacker) x 10 (prop_H1) = 3400


# Set seed for reproducibility
set.seed(12345)
sim_name <- "sim_realistic"

#---------------------------------------------------
# Final simulation conditions

# nvar applies to p-hacking researchers; non-hackers have nvar=1
# note: this approximates a logarithmic progression of nvar:
# floor(exp(seq(log(2), log(200), by = 0.2))) |> unique()

nvar = c(2, 3, 5, 8, floor(exp(seq(log(10), log(150), by = 0.3))) |> unique())
r = seq(0, 0.9, by = 0.3)
d = seq(0.1, 0.8, by = 0.1) # ES under H1; under H0 it's always 0
het = seq(0, 0.4, by = 0.1)
prop_Hacker = c(0.1, seq(0.2, 1, by = 0.2)) # proportion of researchers practicing p-hacking
prop_H1 = seq(0.1, .5, by = 0.1) # proportion of true H1 effects

iter = 10000   # this is fixed
alpha = 0.05 # this is fixed

#---------------------------------------------------

# Create grid of conditions
conditions <- expand.grid(nvar, r, d, prop_Hacker, prop_H1, het, iter, alpha, stringsAsFactors = FALSE)
colnames(conditions) <- c("nvar", "r", "d", "prop_Hacker", "prop_H1", "het", "iter", "alpha")

cat("Total conditions to process:", nrow(conditions), "\n")


#---------------------------------------------------
# Testing simulation conditions

# nvar = c(2, 5, 10, 50, 200)
# r = seq(0, 0.9, by = 0.3)
# d = seq(0.1, 0.8, by = 0.1) # ES under H1; under H0 it's always 0
# het = seq(0, 0.3, by = 0.1)
# prop_Hacker = seq(0.2, 0.8, by = 0.2) # proportion of researchers practicing p-hacking, 0 = best case, 1 = worst case
# prop_H1 = seq(0.1, 0.5, by = 0.1) # proportion of true H1 effects
#
# iter = 1000   # this is fixed
# alpha = 0.05 # this is fixed


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
# Helper function: Write results of each condition into a shared directory structure
# (to avoid 300000 files in a single dir)

get_cache_filename <- function(cond_row, base_dir, algo = "xxhash64") {
  stopifnot(is.data.frame(cond_row), nrow(cond_row) == 1)

  # 1. Stable hash of the row -----------------------------------------------
  # Serialize the row so that *all* columns influence the hash
  hash_val <- digest::digest(cond_row, algo = algo)

  # 2. Two-level shard (00–ff / 00–ff) --------------------------------------
  shard1 <- substr(hash_val, 1, 2)
  shard2 <- substr(hash_val, 3, 4)
  dir_path  <- file.path(base_dir, shard1, shard2)

  if (!dir.exists(dir_path))
    dir.create(dir_path, recursive = TRUE, showWarnings=FALSE)

  # 3. Write the result ------------------------------------------------------
  file_path <- file.path(dir_path, paste0(hash_val, ".rds"))
  return(file_path)
}



save_cached_file <- function(object, file, compress = "xz") {
  saveRDS(object, file, compress = compress)
}




# Progress tracking setup
cat("Processing", nrow(conditions), "conditions...\n")
dir.create(paste0("../simulations/sim-results/", sim_name), showWarnings=FALSE)
start_time <- Sys.time()

# Run parallel simulation with reproducible RNG
simres <- foreach(i = 1:nrow(conditions),
                  .combine = rbind,
                  .packages = c("fitPCurve", "rio")) %dorng% {

                    intermediate_fn <- get_cache_filename(conditions[i, ], paste0("../simulations/sim-results/", sim_name))

                    if (file.exists(intermediate_fn)) {
                      skip_msg <- paste0("Condition ", i, " is skipped ...")
                      warning()
                      write(skip_msg, file = paste0("../simulations/sim-results/", sim_name, "/00-", sim_name,"_progress.txt"), append = TRUE)
                      next;
                    }

                    ps <- tryCatch({
                      sim.multDVhack(nvar = conditions[i, 1],
                                     r = conditions[i, 2],
                                     d = conditions[i, 3],
                                     prop_Hacker = conditions[i, 4],
                                     prop_H1 = conditions[i, 5],
                                     het = conditions[i, 6],
                                     iter = conditions[i, 7],
                                     alpha = conditions[i, 8])
                    }, error = function(e) {
                      cat(paste0("Simulation failed: ", e))
                      return(matrix(NA, nrow=iter, ncol=3))
                    }
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
                    write(progress_msg, file = paste0("../simulations/sim-results/", sim_name, "/00-", sim_name,"_progress.txt"), append = TRUE)

                    # write intermediate file
                    save_condition_result(res_i, file=intermediate_fn)

                    # return the result
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
export(simres, paste0("../simulations/sim-results/", sim_name, ".csv"))
stop();


#---------------------------------------------------
# Optionally: Aggregate intermediate files
# (e.g., if the computation has been interrupted due to an error)

library(data.table)

im_files <- list.files(paste0("../simulations/sim-results/", sim_name), pattern=paste0(sim_name,"_\\d*.csv"), full.names = TRUE)

combined_data <- rbindlist(lapply(im_files, fread))
combined_data$V1 <- NULL
result_matrix <- as.matrix(combined_data)

export(result_matrix, paste0("../simulations/sim-results/", sim_name, ".csv"))
