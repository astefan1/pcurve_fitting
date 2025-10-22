# ==============================================================================
# Compute G-squared (Goodness of fit statistic)
# ==============================================================================

#' Compute G-squared goodness of fit statistic between one observed p-curve and
#' one or many expected p-curves.
#'
#' @description Calculates the G-squared statistic between a single observed
#' vector of probabilities (empirical distribution, observed values) and either a single expected vector or a matrix of
#' expected probabilities (each row is an expected probability; theoretical/simulated distribution). 
#' @param observed Numeric vector of probabilities (must sum to 1 within tolerance).
#' @param expected Numeric vector of probabilities (length = length(observed)),
#'   or a numeric matrix with \code{ncol(expected) == length(observed)} and
#'   each row summing to 1 within tolerance. If expected contains zeros, then these will be replaced by an arbitrary small number (tol). 
#' @param n Positive scalar total sample size used to scale the statistic.
#' @param tol Numeric tolerance for probability sum checks (default 1e-10).
#' @return If \code{expected} is a vector, a single numeric value.
#'   If \code{expected} is a matrix, a numeric vector of length \code{nrow(expected)}
#'   (names taken from \code{rownames(expected)} if present).
#' @export
#' 
g2 <- function(observed, expected, n, tol = 1e-10){
  
  if (is.data.frame(expected)) expected <- as.matrix(expected)  # ensures matrix methods work for data.frame input
  
  # Basic validation -----------------------------------------------------------
  if (!is.numeric(observed) || anyNA(observed) || any(observed < 0))
    stop("`observed` must be a nonnegative numeric vector without NA.")
  
  if (!is.numeric(expected) || anyNA(expected))
    stop("`expected` must be numeric without NA (vector or matrix).")
  
  if (length(n) != 1 || !is.finite(n) || n <= 0)
    stop("`n` must be a single positive, finite number.")
  
  ref_sum <- sum(observed)
  if (abs(ref_sum - 1) > tol)
    stop(sprintf("Probabilities in `observed` must sum to 1 (got %.10f).", ref_sum))
  
  if (0 %in% observed)
    observed[which(observed == 0)] <- tol
  
  if (0 %in% expected)
    expected[which(expected == 0, arr.ind = TRUE)] <- tol # replace zeros in expected with arbitrary small number
  
  # Handle vector vs. matrix cases ---------------------------------------------
  
  if (is.matrix(expected)) { # Matrix case: rows are expected vectors
    
    # expected matrix validation
    
    if (ncol(expected) != length(observed)) {
      stop(sprintf("`ncol(expected)` (%d) must equal length(observed) (%d).",
                   ncol(expected), length(observed)))
    }
    
    if (any(expected <= 0))
      stop("All entries of `expected` must be strictly positive for chi-square.")
    
    row_sums <- rowSums(expected)
    bad <- which(abs(row_sums - 1) > tol)
    if (length(bad)) {
      stop(sprintf("Row(s) of `expected` do not sum to 1 within tolerance: %s",
                   paste(bad, collapse = ", ")))
    }
    
    # Compute G2
    
    out <- apply(expected, 1, function(x) 2*n*sum(observed*log(observed/x)))
    
    # Preserve row names if present
    if (!is.null(rownames(expected))) names(out) <- rownames(expected)
    return(out)
    
  } else { # Vector case
    
    expected <- as.numeric(expected)
    if (length(expected) != length(observed))
      stop("`expected` and `observed` must have the same length.")
    
    comp_sum <- sum(expected)
    if (abs(comp_sum - 1) > tol)
      stop(sprintf("Probabilities in `expected` must sum to 1 (got %.10f).", comp_sum))
    
    if (any(expected <= 0))
      stop("All entries of `expected` must be strictly positive for chi-square.")
    
    out <- 2*n*sum(observed*log(observed/expected))
    return(out)
    
  }
  
  
  
}
