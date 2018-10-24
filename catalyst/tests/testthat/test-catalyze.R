context("a more serious catalyze simulation")

test_that("Setting up a simulation with 100 people",{

    ## Setting up people
    T <- 50
    N <- 100
    K <- 2
    agent_data <- matrix(0, nrow = T, ncol = N)
    init_vals <- c(rep(1, 90), rep(2, 10)) # 90 sus, 10 inf
    agent_data[1, ] <- init_vals
    ll <- 1
    trans_fxn <- SI
    nbr_list <- rep(list(1:N), N) ## everyone is a neighbor
    states <- 1:K
    inf_states <- 2
    sus_states <- 1
    sus_inf_arr <- array(0, dim = c(K, K, K))
    theta <- .08
    do_AM <- FALSE # no AM

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

    optim_out <- optimize(f = loglike_CM_SI,
                          interval = c(0, 1), X_mat = X)
    optim_out$min

})
