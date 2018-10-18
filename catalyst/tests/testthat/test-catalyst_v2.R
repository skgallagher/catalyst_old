context("testing catalyst_v2 functions")


test_that("get_totals", {

    T <- 3
    N <- 4
    K <- 3
    agent_data <- matrix(c(1, 1, 2,
                           1, 2, 3,
                           1, 2, 2,
                           2, 3, 3), nrow = T, ncol = N, byrow = FALSE)
    y <- get_totals(agent_data, 1, K)
    expect_equal(y, c(3, 1, 0))
    ##
    y <- get_totals(agent_data, 2, K)
    expect_equal(y, c(1, 2, 1))
    y <- get_totals(agent_data, 3, K)
    expect_equal(y, c(0, 2, 2))


})
