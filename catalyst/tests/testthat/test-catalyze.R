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


test_that("Trying the SIR", {

    ## Setting up people
    T <- 50
    N <- 100
    K <- 3
    agent_data <- matrix(0, nrow = T, ncol = N)
    init_vals <- c(rep(1, 90), rep(2, 10)) # 90 sus, 10 inf
    agent_data[1, ] <- init_vals
    ll <- 1
    trans_fxn <- SIR
    nbr_list <- rep(list(1:N), N) ## everyone is a neighbor
    states <- 1:K
    inf_states <- 2
    sus_states <- 1
    sus_inf_arr <- array(0, dim = c(K, K, K))
    theta <- c(.1, .03)
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
    colnames(X) <- c("S", "I", "R")
    X <- as.data.frame(X)
    X$t <- 0:(nrow(X) - 1)
    plot(X$t, X$S / N, type = "l", lwd =2, col ="blue",
         ylim = c(0,1))
    lines(X$t, X$I / N,  type = "l", lwd =2, col ="red")
    lines(X$t, X$R / N,  type = "l", lwd =2, col ="darkgreen")

})

test_that("test-parallel", {

    ## So many params
    L <- 1
    ncores <- 1
    ncores <- 1
    do_par <- FALSE
    trans_fxn <- SI
    theta <- c(0)
    T <- 50
    ######## Agent data
    agent_data <- matrix(0, nrow = T, ncol = N)
    init_vals <- c(rep(1, 90), rep(2, 10)) # 90 sus, 10 inf
    agent_data[1, ] <- init_vals
######### NBR LIST
    nbr_list <- rep(list(1:N), N) ## everyone is a neighbor
    states <- 1:2
    inf_states <- 2
    sus_states <- 1
    sus_inf_arr = array(0, dim = (1))
    do_AM <- FALSE
    do_keep_agent_data <- TRUE
    do_write_agent_data <- FALSE
    do_write_inits <- FALSE
    writing_dir <- "."


    L <- 2
    out <- simulate_catalyst(L = L,
                        ncores = n_cores,
                        do_par = do_par,
                        trans_fxn = trans_fxn,
                        theta = theta,
                        agent_data = agent_data,
                        nbr_list = nbr_list,
                        states = states,
                        inf_states = inf_states,
                        sus_states = sus_states,
                        sus_inf_arr = sus_inf_arr,
                        do_AM = do_AM,
                        do_keep_agent_data = do_keep_agent_data,
                        do_write_agent_data = do_write_agent_data,
                        do_write_inits = do_write_inits,
                        writing_dir = writing_dir)
    expect_equal(length(out), L)

    expect_equal(out[[1]]$X, out[[2]]$X)

})
