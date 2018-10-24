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


test_that("find nbr indices", {
    inf_inds <- 1:10
    nbr_inds <- 3:5
    x <- find_inf_nbrs(inf_inds, nbr_inds)
    expect_equal(c(3,4,5), x)
    ## different
    inf_inds <- 3
    nbr_inds <- 3:5
    x <- find_inf_nbrs(inf_inds, nbr_inds)
    expect_equal(3, x)
    ## No overlap
    inf_inds <- 3
    nbr_inds <- 4:9
    x <- find_inf_nbrs(inf_inds, nbr_inds)
    expect_equal(0, x)


})

test_that("loglike AM SI", {
    theta <- .5
    T <- 3
    N <- 4
    agent_data <- matrix(c(1, 1, 1, 2,
                           2, 1, 1, 2,
                           2, 2, 1, 2),
                         nrow = T, ncol = 4, byrow = TRUE)
    nbr_list <- list(c(2, 4),
                     c(1),
                     c(0),
                     c(1))
    ## log like of just one
    x <- loglike_AM_SI(theta, agent_data[1:2,], nbr_list)
    loglike_a1 <- log(theta)
    loglike_a2 <- 0
    loglike_a3 <- 0
    loglike_a4 <- 0
    exp_loglike <- loglike_a1 + loglike_a2 + loglike_a3 + loglike_a4
    expect_equal(-x, exp_loglike)

    ## Log like of both time steps
    x <- loglike_AM_SI(theta, agent_data[,], nbr_list)
    loglike_a1 <- 0
    loglike_a2 <- log(theta)
    loglike_a3 <- 0
    loglike_a4 <- 0
    exp_loglike2 <- exp_loglike + loglike_a2
    expect_equal(-x, exp_loglike2)
    


})

