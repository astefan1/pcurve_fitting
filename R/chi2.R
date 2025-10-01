# ==============================================================================
# Compute chi2 goodness of fit statistic
# ==============================================================================

#' Compute Chi2 goodness of fit statistic between two p-curves
#' @description Calculates Chi squared statistic between two vectors
#' @param reference First vector of probabilities (original)
#' @param comparison Second vector of probabilities (fitted values), can also be a matrix with each row being a different comparison
#' @param n Total sample size
#' @export

chi2_old <- function(reference, comparison, n){
  if (round(sum(reference), 6) != 1) {
    stop(paste0("Probabilities in reference do not sum to 1: ", reference))
  }
  if (round(sum(comparison), 6) != 1) {
    stop(paste0("Probabilities in comparison do not sum to 1: ", comparison))
  }
  sum((reference-comparison)^2/comparison)*n
}




# ==============================================================================
# Compute chi2 goodness of fit statistic
# ==============================================================================

#' Compute Chi2 goodness of fit statistic between one reference p-curve and
#' one or many comparison p-curves.
#'
#' @description Calculates the Chi-squared statistic between a single reference
#' vector of probabilities (empirical distribution, observed values) and either a single comparison vector or a matrix of
#' comparisons (each row is a comparison; theoretical/simulated distribution, expected values). 
#' @param reference Numeric vector of probabilities (must sum to 1 within tolerance).
#' @param comparison Numeric vector of probabilities (length = length(reference)),
#'   or a numeric matrix with \code{ncol(comparison) == length(reference)} and
#'   each row summing to 1 within tolerance. If comparison contains zeros, then these will be replaced by an arbitrary small number (1e-8). 
#' @param n Positive scalar total sample size used to scale the statistic.
#' @param tol Numeric tolerance for probability sum checks (default 1e-6).
#' @return If \code{comparison} is a vector, a single numeric value.
#'   If \code{comparison} is a matrix, a numeric vector of length \code{nrow(comparison)}
#'   (names taken from \code{rownames(comparison)} if present).
#' @export
chi2 <- function(reference, comparison, n, tol = 1e-6) {

  if (is.data.frame(comparison)) comparison <- as.matrix(comparison)  # ensures matrix methods work for data.frame input

  # Basic validation -----------------------------------------------------------
  if (!is.numeric(reference) || anyNA(reference) || any(reference < 0))
    stop("`reference` must be a nonnegative numeric vector without NA.")

  if (!is.numeric(comparison) || anyNA(comparison))
    stop("`comparison` must be numeric without NA (vector or matrix).")

  if (length(n) != 1 || !is.finite(n) || n <= 0)
    stop("`n` must be a single positive, finite number.")

  ref_sum <- sum(reference)
  if (abs(ref_sum - 1) > tol)
    stop(sprintf("Probabilities in `reference` must sum to 1 (got %.10f).", ref_sum))
  
  if (0 %in% comparison)
    comparison[which(comparison == 0, arr.ind = TRUE)] <- 1e-8 # replace zeros in comparison with arbitrary small number

  # Handle vector vs matrix cases ---------------------------------------------
  if (is.matrix(comparison)) {
    # Matrix case: rows are comparisons
    if (ncol(comparison) != length(reference)) {
      stop(sprintf("`ncol(comparison)` (%d) must equal length(reference) (%d).",
                   ncol(comparison), length(reference)))
    }

    if (any(comparison <= 0))
      stop("All entries of `comparison` must be strictly positive for chi-square.")

    row_sums <- rowSums(comparison)
    bad <- which(abs(row_sums - 1) > tol)
    if (length(bad)) {
      stop(sprintf("Row(s) of `comparison` do not sum to 1 within tolerance: %s",
                   paste(bad, collapse = ", ")))
    }

    # Efficient identity: sum((r-c)^2/c) = sum(r^2/c) - 1
    # Broadcast reference^2 over rows and divide by each row of comparison.
    r2 <- reference^2
    # Construct a matrix with r2 repeated by row then divide element-wise
    # and sum by row.
    chi_core <- rowSums(matrix(r2, nrow = nrow(comparison), ncol = length(reference),
                               byrow = TRUE) / comparison)
    out <- (chi_core - 1) * n

    # Preserve row names if present
    if (!is.null(rownames(comparison))) names(out) <- rownames(comparison)
    return(out)

  } else {
    # Vector case
    comparison <- as.numeric(comparison)
    if (length(comparison) != length(reference))
      stop("`comparison` and `reference` must have the same length.")

    comp_sum <- sum(comparison)
    if (abs(comp_sum - 1) > tol)
      stop(sprintf("Probabilities in `comparison` must sum to 1 (got %.10f).", comp_sum))

    if (any(comparison <= 0))
      stop("All entries of `comparison` must be strictly positive for chi-square.")

    # Use the same identity for efficiency
    out <- (sum((reference^2) / comparison) - 1) * n
    return(out)
  }
}
