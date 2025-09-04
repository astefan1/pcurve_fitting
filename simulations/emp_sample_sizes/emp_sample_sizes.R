# This function copies code from https://github.com/nicebread/meta-showdown/blob/master/Empirical%20sample%20size%20distributions/sample%20size.R

## ======================================================================
## Read Marszalek data set. Cite as:

# Marszalek, J. M. (2011). Sample size in psychological research over the past 30 years [Data file]. Retrieved from https://mospace.umsystem.edu/xmlui/handle/10355/62220
#
# Marszalek, J. M., Barber, C., Kohlhart, J., & Holmes, C. B. (2011). Sample size in psychological research over the past 30 years. Perceptual and Motor Skills, 112(2), 331-348. doi: 10.2466/03.11.PMS.112.2.331-348
## ======================================================================


library(rio)
library(dplyr)
M.ns <- import("simulations/emp_sample_sizes/MarszalekSamSizPsy-Excel.xls")

n.min <- 5

# "first group": pull out the first column of per-group sample sizes (not the others, because they are not independent from the first)

M.n.all.groups0 <- M.ns %>% select(contains("n_")) %>% unlist %>% as.numeric %>% na.omit
M.n.first.group0 <- M.ns %>% pull("n_A") %>% as.numeric() %>% na.omit %>% as.vector

summary(M.n.first.group0)
summary(M.n.all.groups0)

# remove most extreme outliers
M.n.first.group <- M.n.first.group0[M.n.first.group0 >= n.min]
M.n.all.groups <- M.n.all.groups0[M.n.all.groups0 >= n.min]

summary(M.n.first.group)
summary(M.n.all.groups)

quantile(M.n.first.group, prob=c(.05, .50, .75, .90, .95, .99)) |> round()
