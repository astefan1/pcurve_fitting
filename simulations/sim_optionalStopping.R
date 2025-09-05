# ==============================================================================
# Optional Stopping Simulation
# ==============================================================================

# This script calculates the p-value sequentially after each observation 10,000
# times in each simulation condition
library(fitPCurve)
library(TruncExpFam)
library(rio)

# First simulate dataset of p-values under H1

nMax <- 1063 # maximum N per group = 99th percentile of empirical distribution
groups <- 2 # independent-samples t-test
iter <- 10000
ds <- seq(0, 0.8, by = 0.1)
het <- seq(0, 0.4, by = 0.2)
ES <- expand.grid(ds, het)
set.seed(111092)
ESlist <- apply(ES, 1, function(x) rnorm(iter, x[1], x[2])) # each row in ES (ES+het combi) is a column with iter elements in ESlist

set.seed(111092)
simdata0 <- array(rnorm(prod(nMax, groups, iter)), dim=c(nMax, groups, iter)) # draw from standard normal
simdata <- array(NA, dim=c(nMax, groups, iter)) # placeholder for each effect size
simres <- array(NA, dim=c(nMax, iter, ncol(ESlist))) # this is where the p-values go

# Sequentially compute t and p values for different effect sizes
for(i in 1:ncol(ESlist)){

  print(i)
  simdata <- simdata0

  # Add effect size to one group
  for(j in 1:iter){
    simdata[, 1, j] <- simdata0[, 1, j] + ESlist[j, i]
  }

  # 1:N
  seqOneToNMax <- seq(1:nMax)
  # cumulative mean difference
  X1minX2 <- apply(simdata, 3, function(x) (cumsum(x[,1])/seqOneToNMax)-(cumsum(x[,2])/seqOneToNMax))
  # cumulative variance
  Var1 <- apply(simdata, 3, function(x) cumvar(x[,1]))
  Var2 <- apply(simdata, 3, function(x) cumvar(x[,2]))
  # cumulative pooled standard deviation
  spool <- sapply(1:iter, function(x) sqrt(((seqOneToNMax-1)*Var1[,x] + (seqOneToNMax-1)*Var2[,x])/(seqOneToNMax+seqOneToNMax-2)))

  # cumulative t-value calculation
  denom <- sapply(1:iter, function(x) spool[,x]*sqrt((1/seqOneToNMax) + (1/seqOneToNMax))) # t value denominator
  tval <- X1minX2/denom # t-value

  # cumulative p-value calculation
  pval <- apply(tval, 2, function(x) (1-pt(abs(x), 2*seqOneToNMax-2))*2) # p-value
  simres[,,i] <- pval
}

saveRDS(simres, "simulations/sim-results/optionalStopping_simres.rds")
simres <- readRDS("simulations/sim-results/optionalStopping_simres.rds")


# ==============================================================================
# Then, compute p-curves for different scenarios
# A (prop_H1 > 0 and prop_Hacker > 0): nmin x nmax x stepsize x 3 (het) x 5 (prop_Hacker) x 5 (prop_H1)
# B (prop_H1 = 0 and prop_Hacker = 0): --> skip; same as other simulation
# C (prop_H1 > 0 and prop_Hacker = 0): --> skip; same as other simulation
# D (prop_H1 = 0 and prop_Hacker > 0): nmin x nmax x stepsize x 5 (prop_Hacker) (het fixed to 0)
# ==============================================================================

# Scenario A: 
# ---------------------------------------------------------------------------------

prop_Hacker = c(0.1, seq(0.25, 1, by = 0.25)) # proportion of researchers practicing p-hacking
prop_H1 = c(0.1, 0.2, 0.3, 0.5, 0.7) # proportion of true H1 effects

# based on empirical quantiles of Marzalek: quantile(M.n.first.group, prob=c(.05, .50, .75, .90, .95, .99)) |> round()
nmin <- c(8, 22, 50, 177, 444) 
nmax <- c(22, 50, 177, 444, 1063)

stepsize <- c(1, 5, 10, 50, 100)
ds <- seq(0.1, 0.8, by = 0.1)
het <- seq(0, 0.4, by = 0.2)

conditions <- expand.grid(prop_Hacker, prop_H1, nmin, nmax, stepsize, ds, het)
colnames(conditions) <- c("prop_Hacker", "prop_H1", "nmin", "nmax", "stepsize", "d", "het")

# Delete superfluous conditions
conditions <- conditions[conditions$nmin < conditions$nmax,] # nmin > nmax
conditions <- conditions[conditions$stepsize < (conditions$nmax-conditions$nmin), ] # stepsize > than diff between min and max

# Compute p-curves
conditions[,8:12] <- NA
colnames(conditions)[8:12] <- paste0("p", 1:5)



# Load required packages for parallel processing
library(future)
library(doFuture)
library(doRNG)
library(foreach)

# Function to run optional stopping simulation in parallel
sim_optionalStopping <- function(conditions, n_cores = NA) {
  # Set up parallel backend
  future::plan(sequential)
  if (!is.na(n_cores)) {
    n_cores <- as.integer(n_cores)
  } else {
    n_cores <- max(1L, parallel::detectCores() - 2L)
  }
  message("Using ", n_cores, " worker(s)…")
  
  future::plan(multisession, workers = n_cores)
  doFuture::registerDoFuture()
  
  start_time <- Sys.time()
  message("Processing ", nrow(conditions), " condition rows…")
  
  # Parallel loop using foreach
  results <- foreach(i = seq_len(nrow(conditions)),
                    .combine = rbind,
                    .packages = c("fitPCurve", "TruncExpFam"),
                    .export = c("simres", "ES", "optionalStopping", "compute_pcurve"),
                    .options.future = list(scheduling = 1)
  ) %dorng% {
    
    # Run simulation for this condition
    ps <- optionalStopping(simres = simres, 
                          ES = ES,
                          prop_Hacker = conditions[i, 1],
                          prop_H1 = conditions[i, 2],
                          nmin = conditions[i, 3],
                          nmax = conditions[i, 4],
                          stepsize = conditions[i, 5],
                          d = conditions[i, 6],
                          het = conditions[i, 7],
                          alpha = 0.05)
    
    # Compute p-curve for this condition
    pcurve_results <- compute_pcurve(ps)
    
    # Return row with condition parameters and results
    c(unlist(conditions[i, 1:7]), pcurve_results)
  }
  
  # Clean up
  total_time <- round(difftime(Sys.time(), start_time, units = "mins"), 2)
  cat("Total processing time:", total_time, "minutes\n")
  future::plan(sequential)
  
  # Convert back to data frame format
  results_df <- as.data.frame(results)
  colnames(results_df) <- c(colnames(conditions)[1:7], paste0("p", 1:5))
  
  return(results_df)
}

# Usage: Replace your for loop with this parallel version
# Make sure simres and ES are available in the environment
# As the simres object is really large (2.14 GB), don't have too many parallel workers.

# Increase the maximum size for global objects to accommodate the large simres object
options(future.globals.maxSize = 3 * 1024^3)  # Set to 3 GiB (larger than our 2.14 GiB object)

# This takes about xx minutes with 4 cores
conditions_results <- sim_optionalStopping(conditions, n_cores = 4)

# Save results
export(conditions_results, paste0("simulations/sim-results/sim_optStop_realistic.csv"))

# Scenario D:  (prop_H1 = 0 and prop_Hacker > 0): nmin x nmax x stepsize x 5 (prop_Hacker) (het fixed to 0)
# ---------------------------------------------------------------------------------

prop_Hacker = c(0.1, seq(0.25, 1, by = 0.25)) # proportion of researchers practicing p-hacking
prop_H1 = 0

# based on empirical quantiles of Marzalek: quantile(M.n.first.group, prob=c(.05, .50, .75, .90, .95, .99)) |> round()
nmin <- c(8, 22, 50, 177, 444) 
nmax <- c(22, 50, 177, 444, 1063)
stepsize <- c(1, 5, 10, 50, 100)
ds <- 0
het <- 0

conditions2 <- expand.grid(prop_Hacker, prop_H1, nmin, nmax, stepsize, ds, het)
colnames(conditions2) <- c("prop_Hacker", "prop_H1", "nmin", "nmax", "stepsize", "d", "het")

# Delete superfluous conditions
conditions2 <- conditions2[conditions2$nmin < conditions2$nmax,] # nmin > nmax
conditions2 <- conditions2[conditions2$stepsize < (conditions2$nmax-conditions2$nmin), ] # stepsize > than diff between min and max

# Compute p-curves
conditions2[,8:12] <- NA
colnames(conditions2)[8:12] <- paste0("p", 1:5)

conditions_results2 <- sim_optionalStopping(conditions2, n_cores = 4)

# Save results
export(conditions_results2, paste0("simulations/sim-results/sim_optStop_worst.csv"))