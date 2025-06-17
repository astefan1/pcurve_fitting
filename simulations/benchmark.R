# devtools::install_github("hadley/lineprof")
library(lineprof)

f1 <- function() {

  iter = 100
  r = 0.5
  d = 0
  nvar = 200

  # Draw number of observations from empirical distribution
  nobs.group <- round(TruncExpFam::rtruncinvgamma(n = iter, a=5, b=1905, shape=1.15326986, scale=0.04622745))

  # Simulate as many datasets as desired iterations
  dat <- sapply(nobs.group, function(x) .sim.multDV(nobs.group = x, nvar = nvar, r = r, d = d))

  for (i in 1:iter) {
    x=.multDVhack(dat[[i]], dvs = c(2:(nvar+1)), group = 1, strategy = "smallest.sig", alternative = "two.sided", alpha = 0.05)
  }

}

lp <- lineprof(f1())
print(lp)




# this is not very useful, as it does not dive into the functions themselves

f2 <- function() {

  ps <- sim.multDVhack (nvar=200,
                        r=0.5,
                        d=0,
                        strategy = "smallest.sig",
                        iter = 100,
                        alternative = "two.sided",
                        alpha = .05
  )

}

lp <- lineprof(f2())
print(lp)
