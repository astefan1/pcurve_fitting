# ==============================================================================
# Compute root mean squared error (extent of deviation between two p-curves)
# ==============================================================================

#' Compute RMSE between two p-curves
#' @description Calculates square root of mean of squared differences between two vectors
#' @param observed First vector (original)
#' @param expected Second vector (fitted values)
#' @export

rmse_old <- function(observed, expected){
  observed <- unlist(observed)
  expected <- unlist(expected)
  sqrt(mean((observed-expected)^2))
}



# ==============================================================================
# Compute root mean squared error (extent of deviation between two p-curves)
# ==============================================================================

#' Compute RMSE between one observed and one or many expected p-curves
#'
#' @description Calculates the root mean squared error between a single
#' vector of probabilities (empirical distribution, observed values) and either a single expected vector or a matrix of
#' expected probabilities (each row is an expected probability; theoretical/simulated distribution). 
#' @param observed Numeric vector.
#' @param expected Numeric vector of same length as \code{observed}, or a
#'   numeric matrix with \code{ncol(expected) == length(observed)} where
#'   each row is compared to \code{observed}.
#' @param na.rm Logical; if \code{TRUE}, ignore NA values in the computation.
#' @return If \code{expected} is a vector, a single numeric value.
#'   If \code{expected} is a matrix, a numeric vector of length
#'   \code{nrow(expected)} (row names preserved if present).
#' @export
rmse <- function(observed, expected, na.rm = FALSE) {
  observed <- as.numeric(observed)
  if (is.data.frame(expected)) expected <- as.matrix(expected)  # ensures matrix methods work for data.frame input

  if (is.matrix(expected)) {
    if (!is.numeric(expected)) expected <- data.matrix(expected)
    if (ncol(expected) != length(observed)) {
      stop(sprintf("`ncol(expected)` (%d) must equal length(observed) (%d).",
                   ncol(expected), length(observed)))
    }

    # Vectorized over rows: subtract observed from each column, square, rowMeans
    diffs <- sweep(expected, 2, observed, FUN = "-")
    out <- sqrt(rowMeans(diffs * diffs, na.rm = na.rm))

    if (!is.null(rownames(expected))) names(out) <- rownames(expected)
    return(out)
  } else {
    expected <- as.numeric(expected)
    if (length(expected) != length(observed)) {
      stop("`expected` and `observed` must have the same length.")
    }
    return(sqrt(mean((observed - expected)^2, na.rm = na.rm)))
  }
}
