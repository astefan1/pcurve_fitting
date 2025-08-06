###############################################################################
# Simulation wrapper that runs the p‑curve study with the **future** backend  #
# -------------------------------------------------------------------------- #
#  * Replaces doParallel/PSOCK cluster with future + doFuture (multisession)  #
#  * Keeps doRNG for reproducible parallel RNG streams                        #
#  * Minimises memory footprint by exporting only the symbols the workers     #
#    actually need and letting each worker process one task at a time.        #
#  * Produces a per‑worker progress file to avoid write‑contention.           #
###############################################################################

# ── Packages ────────────────────────────────────────────────────────────────
library(future)      # parallel backend (multisession ‑> local processes)
library(doFuture)    # glue between foreach() and future
library(doRNG)       # reproducible %dorng% streams
library(foreach)     # foreach loop API
library(rio)         # I/O helpers
# plus whatever your simulation functions live in:
# library(fitPCurve)
# source("helper‑functions.R")


sim_pcurve <- function(sim_name, conditions, n_cores = NA) {
  # ── 0. Reset any existing future strategy ────────────────────────────────
  future::plan(sequential)                # make sure nothing is left running
  if (!is.na(n_cores)) {
    n_cores <- as.integer(n_cores)
  } else {
    # leave two cores for the operating system / desktop usage
    n_cores <- max(1L, parallel::detectCores() - 2L)
  }
  message("Using ", n_cores, " worker(s)…")

  # ── 1. Set up the multisession future backend ────────────────────────────
  # Each worker is an independent R session started on this machine.
  future::plan(multisession, workers = n_cores)
  doFuture::registerDoFuture()

  # ── 2. Prep output directories & bookkeeping ─────────────────────────────
  out_dir <- file.path("..", "simulations", "sim-results", sim_name)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # use one progress file per *worker* to avoid file‑locking headaches
  worker_progress_file <- function() {
    # will be evaluated inside the worker so Sys.getpid() is unique there
    file.path(out_dir, sprintf("progress_%s_%d.txt", sim_name, Sys.getpid()))
  }

  start_time <- Sys.time()
  message("Processing ", nrow(conditions), " condition rows…")

  # ── 3. Parallel simulation loop ──────────────────────────────────────────
  simres <- foreach(i = seq_len(nrow(conditions)),
                    .combine   = rbind,
                    .packages  = c("fitPCurve", "rio"),
                    .export    = c("sim.multDVhack", "compute_pcurve",
                                   "get_cache_filename", "save_cached_file"),
                    .options.future = list(scheduling = 1)  # one task at a time
  ) %dorng% {

    # inside the worker -----------------------------------------------------
    progress_file <- worker_progress_file()

    # skip if an intermediate result already exists ------------------------
    intermediate_fn <- get_cache_filename(conditions[i, ], out_dir)
    if (file.exists(intermediate_fn)) {
      msg <- sprintf("Condition %d is skipped …", i)
      write(msg, file = progress_file, append = TRUE)
      return(NULL)   # 'foreach' will ignore NULL contributions
    }

    # run the expensive simulation ----------------------------------------
    ps <- tryCatch(
      sim.multDVhack(nvar         = conditions[i, 1],
                     r            = conditions[i, 2],
                     d            = conditions[i, 3],
                     prop_Hacker  = conditions[i, 4],
                     prop_H1      = conditions[i, 5],
                     het          = conditions[i, 6],
                     iter         = conditions[i, 7],
                     alpha        = conditions[i, 8]),
      error = function(e) {
        warning("Simulation failed: ", conditionMessage(e))
        return(matrix(NA_real_, nrow = conditions[i, 7], ncol = 3))
      })

    # p‑curve stats for the three strategies -------------------------------
    res_pcurve <- matrix(c(compute_pcurve(ps[, 1]),
                           compute_pcurve(ps[, 2]),
                           compute_pcurve(ps[, 3])),
                         byrow = TRUE, ncol = 5)

    # bring everything together --------------------------------------------
    res_i <- cbind(
      cbind(matrix(rep(conditions[i, ], each = 3), nrow = 3, byrow = FALSE), 1:3),
      res_pcurve)
    colnames(res_i) <- c(colnames(conditions), "strategy", paste0("p", 1:5))

    # intermediate cache & progress note -----------------------------------
    save_cached_file(res_i, file = intermediate_fn)
    prog_msg <- sprintf("Completed %d/%d (%.1f%%) at %s",
                        i, nrow(conditions), 100 * i / nrow(conditions), Sys.time())
    write(prog_msg, file = progress_file, append = TRUE)

    res_i
  }

  # ── 4. Post‑processing & cleanup ─────────────────────────────────────────
  total_time <- round(difftime(Sys.time(), start_time, units = "mins"), 2)
  cat("Total processing time:", total_time, "minutes\n")

  future::plan(sequential)               # return to default strategy
  return(simres)
}



#' Combine all .rds matrices in a directory tree (including all subfolders)
#'
#' @param root   Character. Path to the top‑level directory that contains
#'               the .rds files (possibly nested in sub‑folders).
#' @param quiet  Logical. If FALSE (default) prints a short progress message.
#'
#' @return A single matrix created with `rbind()` in the order the files
#'         are returned by `list.files()`.
collect_rds <- function(root, quiet = TRUE) {
  stopifnot(dir.exists(root))

  rds_files <- list.files(root,
                          pattern = "\\.rds$",  # ends with .rds
                          recursive = TRUE,
                          full.names = TRUE)

  if (length(rds_files) == 0L) {
    stop("No .rds files found under ", root)
  }

  if (!quiet) {
    message("Found ", length(rds_files), " file(s)… reading:")
  }

  # Read each file and collect matrices in a list
  mats <- vector("list", length(rds_files))
  for (i in seq_along(rds_files)) {
    if (!quiet) message(sprintf("  [%d/%d] %s", i, length(rds_files), rds_files[i]))
    mats[[i]] <- readRDS(rds_files[i]) |> as.matrix()
  }

  # Sanity‑check: all matrices must have the same number of columns
  ncols <- vapply(mats, ncol, integer(1))
  if (length(unique(ncols)) != 1L) {
    stop("Matrices have differing numbers of columns: ", paste(unique(ncols), collapse = ", "))
  }
  cn <- lapply(mats, colnames)
  if (!all(vapply(cn[-1], identical, logical(1), y = cn[[1]]))) {
    stop("Matrices differ in colnames.")
  }

  # Combine with rbind()
  ncol0 <- ncols[1]
  big_mat <- do.call(rbind, mats)

  big_mat
}
