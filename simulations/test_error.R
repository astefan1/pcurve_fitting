options(error=recover)

# repeat simulation until an erorr occurs ...
for (i in 1:10000) {

    print(paste0("Running simulation ", i, "/10000"))
    ps <- sim.multDVhack(nvar = 149,
                       r = 0.3,
                       d = 0,
                       het = 0,
                       iter = 1000,
                       alpha = 0.05)


}

