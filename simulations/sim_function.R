library(doParallel)
library(doRNG)
library(foreach)
library(rio)




sim_pcurve <- function(sim_name, conditions, n_cores = NA) {

  # Prepare the parallel processing

  # Clean up any existing parallel backends
  try(stopCluster(cl), silent = TRUE)
  stopImplicitCluster()
  registerDoSEQ()  # Reset to sequential

  if (is.na(n_cores)) {
    # How many cores does your CPU have?
    # reduce by 2 cores to keep them for the OS
    n_cores <- detectCores() - 2
    cat("Available cores:", n_cores, "\n")
  }

  progress_file <- paste0("../simulations/sim-results/", sim_name, "/00-", sim_name,"_progress.txt")

  # Setup parallel cluster, one log per worker for error detection
  cl <- makeCluster(n_cores, type = "PSOCK", outfile = "worker_%a.log")
  registerDoParallel(cl)

  # Progress tracking setup
  cat("Processing", nrow(conditions), "conditions...\n")
  dir.create(paste0("../simulations/sim-results/", sim_name), showWarnings=FALSE)
  start_time <- Sys.time()

  # Run parallel simulation with reproducible RNG
  simres <- foreach(i = 1:nrow(conditions),
                    .combine = rbind,
                    .packages = c("fitPCurve", "rio"),
                    # releases mem between tasks
                    .options.snow = list(preschedule = FALSE)) %dorng% {

                      intermediate_fn <- get_cache_filename(conditions[i, ], paste0("../simulations/sim-results/", sim_name))

                      if (file.exists(intermediate_fn)) {
                        skip_msg <- paste0("Condition ", i, " is skipped ...")
                        warning(skip_msg)
                        write(skip_msg, file = progress_file, append = TRUE)
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
                      write(progress_msg, file = progress_file, append = TRUE)

                      # write intermediate file
                      save_cached_file(res_i, file=intermediate_fn)

                      # return the result
                      res_i
                    }

  # Calculate total time
  total_time <- paste0(
    "Total processing time: ",
    difftime(Sys.time(), start_time, units = "mins") |> round(2),
    " minutes\n")

  write(total_time, file = progress_file, append = TRUE)

  # Clean shutdown
  stopCluster(cl)

  return(simres)
}

