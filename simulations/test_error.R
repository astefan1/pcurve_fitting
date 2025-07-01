# Error in { : task 193 failed - "missing value where TRUE/FALSE needed"

options(error=recover)

# repeat simulation until an erorr occurs ...
for (i in 1:10000) {

    print(paste0("Running simulation ", i, "/10000"))
  system.time({
    ps <- sim.multDVhack(nvar = 13,
                       r = 0.3,
                       d = 0,
                       het = 0,
                       iter = 10000,
                       alpha = 0.05)
  })

}


