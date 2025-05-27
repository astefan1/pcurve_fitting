

#' Plot original and fitted p-curves
#' @description Create a plot displaying the original and a number of fitted p-curves
#' @param simdat Result of simulation function
#' @param poriginal Original p-curve (vector)
#' @param ncurves Number of best fitting pcurves to display

plot_pcurves <- function(simdat, poriginal){

  # Compute RMSEs between original and simulated
  simdat$rmses <- apply(simdat[, 10:14], 1, function(x) rmse(x, poriginal))

  # Order by RMSE and extract ncurves
  simdat <- simdat[order(simdat$rmses)[1:5],c(1,10:15)]

  plotdat <- data.frame(pval = seq(0.01, 0.05, by = 0.01),
                        yval = poriginal)

  ggplot(plotdat, aes(x = pval)) +
    geom_line(aes(y = poriginal), linewidth=1.5) +
    theme_bw() +
    ylim(c(0,100)) +
    labs(x = "p-value",
         y = "Percentage of p-values") +
    theme(axis.title = element_text(size = 25),
          axis.text = element_text(size = 15)) +
    geom_line(aes(y = as.numeric(simdat[1, 2:6]))) +
    geom_line(aes(y = as.numeric(simdat[2, 2:6]))) +
    geom_line(aes(y = as.numeric(simdat[3, 2:6]))) +
    geom_line(aes(y = as.numeric(simdat[4, 2:6]))) +
    geom_line(aes(y = as.numeric(simdat[5, 2:6])))

}
