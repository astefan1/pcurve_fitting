# ==============================================================================
# Define set of p-curves to fit
# ==============================================================================

sotola <- c(68, 9, 14, 5, 5)
wetzels <- c(69, 11, 7, 7, 5)
simonsohn <- c(45, 23, 14, 5, 14)

write.csv(cbind(sotola, wetzels, simonsohn), "pcurves-to-fit.csv")
