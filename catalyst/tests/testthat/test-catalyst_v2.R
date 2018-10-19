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

test_that("summarize_agents", {
    N <- 4
    T <- 4
    K <- 3
    agent_data <- matrix(c(1, 1, 1, 2,
                           1, 1, 2, 3,
                           2, 1, 2, 3,
                           3, 1, 3, 3), byrow = TRUE,
                         nrow = T, ncol = N)
    exp_D <- array(0, dim = c(T-1, K, K))
    exp_D[1,  ,] <- matrix(c(2, 1, 0,
                             0, 0, 1,
                             0, 0, 0),
                           nrow = K, ncol = K, byrow = TRUE)
    exp_D[2, ,] <- matrix(c(1, 1, 0,
                            0, 1, 0,
                            0, 0, 1),
                          nrow = K, ncol = K, byrow = TRUE)
    exp_D[3, ,] <- matrix(c(1, 0, 0,
                            0, 0, 2,
                            0, 0, 1),
                          nrow = K, ncol = K, byrow = TRUE)
    z <- summarize_agent_data(agent_data, K)
    expect_equal(z$D, exp_D)
               

})

test_that("D to X mat", {
    N <- 4
    T <- 4
    K <- 3
    agent_data <- matrix(c(1, 1, 1, 2,
                           1, 1, 2, 3,
                           2, 1, 2, 3,
                           3, 1, 3, 3), byrow = TRUE,
                         nrow = T, ncol = N)
    exp_D <- array(0, dim = c(T-1, K, K))
    exp_D[1,  ,] <- matrix(c(2, 1, 0,
                             0, 0, 1,
                             0, 0, 0),
                           nrow = K, ncol = K, byrow = TRUE)
    exp_D[2, ,] <- matrix(c(1, 1, 0,
                            0, 1, 0,
                            0, 0, 1),
                          nrow = K, ncol = K, byrow = TRUE)
    exp_D[3, ,] <- matrix(c(1, 0, 0,
                            0, 0, 2,
                            0, 0, 1),
                          nrow = K, ncol = K, byrow = TRUE)

    exp_X <- matrix(c(3, 1, 0,
                      2, 1, 1,
                      1, 2, 1,
                      1, 0, 3), byrow = TRUE, ncol = K, nrow = T)
    x <- D_to_X_mat(exp_D, init_X = c(3, 1, 0))
    expect_equal(x, exp_X)

})


test_that("catalyze", {
    
    ll <- 1
    trans_fxn <- SI
    ## Important params
    N <- 10
    K <- 2
    T <- 4
    nbr_list <- rep(list(1:N), N)  ## everyone is a neighbor
    agent_data <- matrix(0, nrow = T, ncol = N)
    inits <- c(rep(1, N-1), 2)
    agent_data[1, ] <- inits
    states <- 1:K
    inf_states <- 2
    sus_states <- 1
    sus_inf_arr <- array(0, dim = c(K, K, K))
    theta <- 0 # no one should be infected
    do_AM <- FALSE # no AM

    ## Running
    out <- catalyze(ll, trans_fxn,
                    theta,
                    agent_data,
                    nbr_list,
                    states,
                    inf_states,
                    sus_states,
                    sus_inf_arr,
                    do_AM)
    X <- D_to_X_mat(out$D, out$init_X)

    exp_X <- matrix(c(9, 1,
                      9, 1,
                      9, 1,
                      9, 1), byrow = TRUE, ncol =2)
    expect_equal(exp_X, X)

    ###########################

})
