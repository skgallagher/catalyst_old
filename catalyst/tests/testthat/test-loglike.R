context("loglike")

test_that("loglike CM SI", {
    theta <- .9
    X_mat <- matrix(c(9, 1,
                      7, 3,
                      6, 9),
                    byrow = TRUE,
                    ncol = 2)



    exp_loglike <- numeric(3)
    N <- sum(X_mat[1,])
    for(tt in 2:3){
        p <- theta * X_mat[tt-1, 2] / N
        delta_s <- X_mat[tt-1, 1 ] - X_mat[tt,1 ]
        const <- log(choose(X_mat[tt-1,1], delta_s))
        pos <- delta_s * log(p)
        neg <- X_mat[tt, 1] * log(1 - p)
        exp_loglike[tt-1] <- const +
            pos + neg                             
        
    }
    z <- loglike_CM_SI(theta, X_mat)
    expect_equal(-z, sum(exp_loglike))

    min <- optimize(f = loglike_CM_SI,
                    interval = c(0, 1), X_mat = X_mat)
    x <- seq(0, 1, by = .01)
    y <- sapply(x, loglike_CM_SI, X_mat = X_mat)
   plot(x, y)
   abline(v = min$min)
})
