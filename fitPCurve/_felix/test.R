library(rockchalk)
r <- 0 # Correlation between DVs
n <- 10000 # Total sample size (group sample size = n/2)
d <- 10 # Effect size (Cohen's d)
dat <- mvrnorm(n = n,
               mu = c(0,0),
               Sigma = matrix(c(1,r,r,1), byrow = TRUE, nrow = 2))
dat <- as.data.frame(dat)
dat$x <- rep(c(0,1), each = n/2)
dat$V1_eff <- dat$V1+dat$x*d
dat$V2_eff <- dat$V2+dat$x*d

apply(dat, 2, mean)
apply(dat, 2, sd)
cor(dat) |> round(2)


dat <- mvrnorm(n = n,
               mu = c(d,0),
               Sigma = matrix(c(1,r,r,1), byrow = TRUE, nrow = 2))
dat <- as.data.frame(dat)
dat$x <- rep(c(0,1), each = n/2)

apply(dat, 2, mean)
apply(dat, 2, sd)
cor(dat) |> round(2)
