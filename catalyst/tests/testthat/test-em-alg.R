context("please work o EM")
library(testthat)


test_that("just work darn it", {



    library(surveillance)
    data(hagelloch)
    head(hagelloch.df)
    X <- round(hagelloch.df$tI) + 1
    T <- 55 # literally don't care about that last guy
    
    X <- X[-which(X == 1)] # we only care about susceptibles and not patient 0

    

    ## But first some testing
    wi <- .8
    p1i <- .2
    p2i <- .8
    N <- 100
    Z <- rbinom(n = N, size = 1, prob = wi)
    X <- 1 + rbinom(n = N, size = 1, prob = p1i * Z + (1-Z) * p2i)
    T <- 2


    p1_guess <- .2
    p2_guess <- .3
    w_guess <- .8
    T <- 2
    devtools::load_all()
    out <- EM_SI(p1_guess, p2_guess, w_guess,
                 X, T)

###################



    ############################

################## hags

    data(hagelloch)
    head(hagelloch.df)
    X <- round(hagellochou.df$tI) + 1
    T <- 55 # literally don't care about that last guy
    r <- runif(n=2)
    p1_guess <- r[1]
    p2_guess <- r[2]
    w_guess <- .7
    T <- 2
    devtools::load_all()
    out <- EM_SI(p1_guess, p2_guess, w_guess,
                 X, T)
    

    })

        
