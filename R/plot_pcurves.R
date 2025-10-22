alpha_ramp <- function(x, min_alpha=0.05, max_alpha=1, min=5, max=1000) {
  # raw ramp
  a <- max_alpha + (min_alpha-1) * (x - min) / (max - min)
  # clamp between 0.2 and 1
  pmin(max_alpha, pmax(min_alpha, a))
}


#' Plot original and fitted p-curves
#' @description Create a plot displaying the original and a number of fitted p-curves
#' @param simdat Result of simulation function
#' @param poriginal Original p-curve (vector)
#' @param GOF Goodness of fit measure: one of "rmse", "chi2", "g2" (default)
#' @param n_best Show only the n best curves. If NA, all are shown
#' @param alpha Alpha transparency for the simulated p-curve lines
#' @param n_studies Number of studies in the original p-curve (only needed for chi2 GOF)
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @importFrom scales percent
#' @export

plot_pcurves <- function(simdat, poriginal, GOF = "g2", n_best=NA, alpha=NA, n_studies=NA){

  stopifnot(all(poriginal >= 0) & all(poriginal <= 1))

  # Compute GOF between original and simulated
  if (GOF=="rmse") {
    simdat$GOF <- rmse(observed=poriginal, expected=simdat[, paste0("p", 1:5)])
  } else if (GOF=="chi2"){
    if (is.na(n_studies)) {
      stop("For chi2 GOF, n_studies must be provided")
    }
    simdat$GOF <- chi2(observed=poriginal, expected=simdat[, paste0("p", 1:5)], n=n_studies)
  } else if (GOF=="g2"){
    if (is.na(n_studies)) {
      stop("For g2 GOF, n_studies must be provided")
    }
    simdat$GOF <- g2(observed=poriginal, expected=simdat[, paste0("p", 1:5)], n=n_studies)
  } else {
    stop("GOF must be one of 'rmse' or 'chi2'")
  }
  simdat$condition <- 1:nrow(simdat)

  # Selection: Order by GOF and extract the n_best curves (or all of them)
  if (is.na(n_best)) {
    n_best <- nrow(simdat)
  }
  simdat_sel <- simdat[order(simdat$GOF)[1:n_best], c("condition", paste0("p", 1:5))]

  simdat_sel_long <- pivot_longer(simdat_sel,
                                  cols = matches("p\\d"),
                                  values_to = "yval")
  simdat_sel_long$pval <- substr(simdat_sel_long$name, 2, 2) |> as.numeric()
  simdat_sel_long$pval <- simdat_sel_long$pval / 100

  plotdat <- data.frame(pval = seq(0.01, 0.05, by = 0.01),
                        yval = unlist(poriginal))

  if (is.na(alpha)) {
    alpha <- alpha_ramp(n_best)
  }

  p1 <- ggplot(simdat_sel_long, aes(y = yval, x=pval, group=condition)) +
    theme_bw() +
    scale_y_continuous(
      labels = percent,   # show labels as percentages
      limits = c(0, 1)    # restrict range from 0 to 1
    ) +
    labs(x = "p-value bin",
         y = "Percentage of p-values") +
    theme(axis.title = element_text(size = 25),
          axis.text = element_text(size = 15)) +
    geom_line(color = "steelblue", alpha=alpha) +
    geom_line(data=plotdat, aes(y = yval, x=pval, group=1), linewidth=1)

  p1
}
