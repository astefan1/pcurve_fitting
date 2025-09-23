# ==============================================================================
# Define set of p-curves to fit
# ==============================================================================

sotola <- c(0.656441718, 0.085889571, 0.128834356, 0.036809816, 0.09202454, 163)
wetzels <- c(0.68634064, 0.11467116, 0.07419899, 0.07419899, 0.05059022, 593)
simonsohn <- c(0.454545455, 0.227272727, 0.136363636, 0.045454545, 0.136363636, 22)

pcurves <- data.frame(c("sotola", "wetzels", "simonsohn"),
                 rbind(sotola, wetzels, simonsohn))
colnames(pcurves) <- c("dataset", "p1", "p2", "p3", "p4", "p5", "nsig")
write.csv(pcurves, "reference-pcurves.csv")
