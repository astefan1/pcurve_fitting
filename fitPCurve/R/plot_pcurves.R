

#' Plot original and fitted p-curves
#' @description Create a plot displaying the original and a number of fitted p-curves
#' @param simdat Result of simulation function
#' @param poriginal Original p-curve (vector)
#' @import ggplot2

plot_pcurves <- function(simdat, poriginal, n_best=5){

  # Compute RMSEs between original and simulated
  simdat$rmse <- apply(simdat[, paste0("p", 1:5)], 1, function(x) rmse(x, poriginal))

  simdat$condition <- 1:nrow(simdat)

  # Selection: Order by RMSE and extract the n_best curves
  simdat_sel <- simdat[order(simdat$rmse)[1:n_best], c("condition", paste0("p", 1:5))]

  simdat_sel_long <- pivot_longer(simdat_sel,
                                  cols = matches("p\\d"),
                                  values_to = "yval")
  simdat_sel_long$pval <- substr(simdat_sel_long$name, 2, 2) |> as.numeric()
  simdat_sel_long$pval <- simdat_sel_long$pval / 100

  plotdat <- data.frame(pval = seq(0.01, 0.05, by = 0.01),
                        yval = poriginal)

  p1 <- ggplot(simdat_sel_long, aes(y = yval, x=pval, group=condition)) +
    theme_bw() +
    ylim(c(0,100)) +
    labs(x = "p-value bin",
         y = "Percentage of p-values") +
    theme(axis.title = element_text(size = 25),
          axis.text = element_text(size = 15)) +
    geom_line(color = "steelblue") +
    geom_line(data=plotdat, aes(y = yval, x=pval, group=1), linewidth=1)

  p1
}
