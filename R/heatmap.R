#' Heatmap of a Specified “rmse” Column
#'
#' Creates a tile-based heatmap of the chosen dependent variable (`DV`)
#' across predictors (`nvar`) and response levels (`r`), faceted by
#' `strategy`. The color scale is shared and fixed across all columns
#' whose names contain “rmse”, ensuring comparability.
#'
#' @param sim A data frame containing at least the following columns:
#'   - `nvar`: predictor count (numeric or integer)
#'   - `r`: response level (numeric or factor)
#'   - `strategy`: grouping variable for faceting
#'   - one or more columns with names containing “rmse”
#' @param DV A string naming the column in `sim` to plot (must be one
#'   of the “rmse” columns)
#'
#' @return A `ggplot` object showing the heatmap.
#'
#' @details
#' The fill scale uses the “inferno” viridis palette (reversed) and
#' skews to emphasize the lowest 10% of the RMSE distribution. The
#' limits of the scale are taken from the minimum and maximum across
#' all “rmse” columns in `sim`.
#'
#' @import ggplot2
#' @importFrom dplyr select contains
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' # simulate some data
#' simres <- expand.grid(
#'   nvar = 1:5,
#'   r = seq(0, 1, length.out = 5),
#'   strategy = c("A", "B")
#' ) %>%
#'   mutate(
#'     rmseSotola = runif(n(), 0, 1),
#'     rmseOther  = runif(n(), 0, 2)
#'   )
#'
#' # plot one of the rmse columns
#' heatmap(simres, "rmseSotola")
#' }
p_heatmap <- function(sim, DV) {
  ggplot(sim, aes(x = nvar, y = r, fill = .data[[DV]])) +
    geom_tile() +
    facet_grid(~strategy) +
    scale_fill_viridis_c(
      option = "inferno",
      direction = -1,
      values = c(0, 0.1, 1),
      limits=c(
        min(sim |> select(contains("rmse"))),
        max(sim |> select(contains("rmse")))
      )
    ) +
    scale_y_continuous(breaks = sort(unique(sim$r)))
}
