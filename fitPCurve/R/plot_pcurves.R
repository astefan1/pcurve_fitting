

#' Plot original and fitted p-curves
#' @description Create a plot displaying the original and a number of fitted p-curves
#' @param simdat Result of simulation function
#' @param poriginal Original p-curve (vector)
#' @import ggplot2

plot_pcurves <- function(simdat, poriginal){

  # Compute RMSEs between original and simulated
  simdat$rmses <- apply(simdat[, 10:14], 1, function(x) rmse(x, poriginal))

  # Order by RMSE and extract ncurves
  simdat <- simdat[order(simdat$rmses)[1:5],c(1,10:15)]

  plotdat <- data.frame(pval = seq(0.01, 0.05, by = 0.01),
                        yval = poriginal)

  ggplot2::ggplot(plotdat, ggplot2::aes(x = .data$pval)) +
    ggplot2::geom_line(ggplot2::aes(y = poriginal), linewidth=1.5) +
    ggplot2::theme_bw() +
    ggplot2::ylim(c(0,100)) +
    ggplot2::labs(x = "p-value",
         y = "Percentage of p-values") +
    ggplot2::theme(axis.title = ggplot2::element_text(size = 25),
          axis.text = ggplot2::element_text(size = 15)) +
    ggplot2::geom_line(ggplot2::aes(y = as.numeric(simdat[1, 2:6])), color = "steelblue") +
    ggplot2::geom_line(aes(y = as.numeric(simdat[2, 2:6])), color = "steelblue") +
    ggplot2::geom_line(aes(y = as.numeric(simdat[3, 2:6])), color = "steelblue") +
    ggplot2::geom_line(aes(y = as.numeric(simdat[4, 2:6])), color = "steelblue") +
    ggplot2::geom_line(aes(y = as.numeric(simdat[5, 2:6])), color = "steelblue")

}
