# ==============================================================================
# Compute root mean squared error (extent of deviation between two p-curves)
# ==============================================================================

#' Compute RMSE between two p-curves
#' @description Calculates square root of mean of squared differences between two vectors
#' @param reference First vector (original)
#' @param comparison Second vector (fitted values)
#' @export

rmse_old <- function(reference, comparison){
  reference <- unlist(reference)
  comparison <- unlist(comparison)
  sqrt(mean((reference-comparison)^2))
}



# ==============================================================================
# Compute root mean squared error (extent of deviation between two p-curves)
# ==============================================================================

#' Compute RMSE between one reference and one or many comparison p-curves
#'
#' @description Calculates the root mean squared error between a single
#' reference vector and either a single comparison vector or a matrix of
#' comparisons (each row is a comparison).
#' @param reference Numeric vector.
#' @param comparison Numeric vector of same length as \code{reference}, or a
#'   numeric matrix with \code{ncol(comparison) == length(reference)} where
#'   each row is compared to \code{reference}.
#' @param na.rm Logical; if \code{TRUE}, ignore NA values in the computation.
#' @return If \code{comparison} is a vector, a single numeric value.
#'   If \code{comparison} is a matrix, a numeric vector of length
#'   \code{nrow(comparison)} (row names preserved if present).
#' @export
rmse <- function(reference, comparison, na.rm = FALSE) {
  reference <- as.numeric(reference)
  comparison <- as.matrix(comparison)  # ensures matrix methods work for vector input

  if (is.matrix(comparison)) {
    if (!is.numeric(comparison)) comparison <- data.matrix(comparison)
    if (ncol(comparison) != length(reference)) {
      stop(sprintf("`ncol(comparison)` (%d) must equal length(reference) (%d).",
                   ncol(comparison), length(reference)))
    }

    # Vectorized over rows: subtract reference from each column, square, rowMeans
    diffs <- sweep(comparison, 2, reference, FUN = "-")
    out <- sqrt(rowMeans(diffs * diffs, na.rm = na.rm))

    if (!is.null(rownames(comparison))) names(out) <- rownames(comparison)
    return(out)
  } else {
    comparison <- as.numeric(comparison)
    if (length(comparison) != length(reference)) {
      stop("`comparison` and `reference` must have the same length.")
    }
    return(sqrt(mean((reference - comparison)^2, na.rm = na.rm)))
  }
}
