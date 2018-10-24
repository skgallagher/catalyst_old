context("transmission functions")


test_that("SI transmission", {

    X <- c(10, 10)
    theta <- .4
    mat <- SI(X, theta)
    exp_mat <- matrix(c(10 - 2, 2,
                      0, 10), byrow = TRUE, ncol = 2, nrow = 2)
    expect_equal(exp_mat, mat)

})


test_that("extract_prob_trans", {

    mat <- matrix(c(8, 2,
                        0, 10), byrow = TRUE, ncol = 2, nrow = 2)
    vec <- c(2, 10)
    exp_mat <- matrix(c(.8, .2,
                        0, 1), byrow = TRUE, nrow = 2)
    X <- c(10, 10)
    theta <- .4
    y <- extract_prob_trans(SI, X, theta)
    expect_equal(y, exp_mat)

})


test_that("base to agent_probs", {
    base_probs <- matrix(c(1, 0, 0,
                           0, 1, 0,
                           0, 0, 1), byrow = TRUE, ncol = 3)
    agent_data <- matrix(c(1, 1, 1, 2,
                           2, 1, 1, 3,
                           3, 2, 2, 3,
                           3, 3, 3, 3),
                         byrow = TRUE, ncol = 4)
    ## tt = 1
    tt <- 1
    exp_ap <- matrix(c(1, 0, 0,
                       1, 0, 0,
                       1, 0, 0,
                       0, 1, 0),
                     byrow = TRUE, ncol = 3)
    ap <- base_to_agent_probs(base_probs, agent_data, tt = tt)
    expect_equal(exp_ap, ap)

    ## tt = 2
    tt <- 2
    exp_ap <- matrix(c(0, 1, 0,
                       1, 0, 0,
                       1, 0, 0,
                       0, 0, 1),
                     byrow = TRUE, ncol = 3)
    ap <- base_to_agent_probs(base_probs, agent_data, tt = tt)
    expect_equal(exp_ap, ap)

     ## tt = 2
    tt <- 4
    exp_ap <- matrix(c(0, 0, 1,
                       0, 0, 1,
                       0, 0, 1,
                       0, 0, 1),
                     byrow = TRUE, ncol = 3)
    ap <- base_to_agent_probs(base_probs, agent_data, tt = tt)
    expect_equal(exp_ap, ap)



})


test_that("Test estimate SI", {
    theta <- 1
    X_init <- c(9, 1)
    T <- 3
    
    out <- estimate_transmission(theta, X_init, T = T)
    X2 <- c(9 - theta * 9/10, 1 + theta * 9/10)
    expect_equal(X2, out[2,])
    delta <- theta * X2[1] * X2[2] / 10
    X3 <- c(X2[1] - delta, X2[2] + delta)
    expect_equal(X3, out[3,])
    

})
