context("loglike")

test_that("loglike CM SI", {
    theta <- .9
    X_mat <- matrix(c(9, 1,
                      7, 3,
                      4, 6,
                      0, 10),
                    byrow = TRUE,
                    ncol = 2)
    D_mat <- array(0, dim = c(3, 2, 2))
    D_mat[1,,] <- matrix(c(7, 2,
                           0, 1),
                         byrow = TRUE,
                         ncol = 2)
    D_mat[2,,] <- matrix(c(4, 3,
                           0, 3),
                         byrow = TRUE,
                         ncol = 2)
    D_mat[3,,] <- matrix(c(0, 4,
                           0, 6),
                         byrow = TRUE,
                         ncol = 2)


    exp_loglike <- numeric(3)
    for(tt in 2:4){
        delta_s <- X_mat[tt-1, 1 ] - X_mat[tt,1 ]
        const <- log(choose(X_mat[tt-1,1], delta_s))
        pos <- delta_s * log(theta)
        neg <- X_mat[tt,1] * log(1 - theta)
        exp_loglike[tt-1] <- const +
            pos + neg                             
        
    }
    z <- loglike_CM_SI(theta, X_mat)
    expect_equal(-z, sum(exp_loglike))

    min <- optimize(f = loglike_CM_SI,
                    interval = c(0, 1), X_mat = X_mat)
    x <- seq(0, 1, by = .01)
    y <- sapply(x, loglike_CM_SI, X_mat = X_mat)
    ## plot(x, y)
    ## abline(v = min$min)
})
