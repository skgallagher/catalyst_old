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


test_that("actually testing out catalyst_v2", {


})
