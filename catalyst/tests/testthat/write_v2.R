context("Writing is fun")


test_that("write_agent_data", {

    T <- 3
    N <- 4
    K <- 2
    agent_data <- matrix(c(1, 1, 1, 2,
                           2, 1, 1, 2,
                           2, 1, 1, 2),
                         byrow = TRUE, ncol = K, nrow = T)

    writing_dir <- "~/tmp"
    if(!dir.exists(writing_dir)) dir.create(writing_dir)

    ## ll = 1
    ll <- 1
    out <- write_agent_data(writing_dir, ll, agent_data)
    expect_true(out)

    ## appending
    ll <- 2
    out <- write_agent_data(writing_dir, ll, agent_data)
    expect_true(out)
    unlink(writing_dir, recursive = TRUE)
    

})

test_that("write inits", {
    ## So many params
    L = 5
    ncores <- 1
    do_par <- TRUE
    trans_fxn <- SI
    theta <- .4
    agent_data <- 1
    nbr_list <- 1
    states <- 1:2
    inf_states <- 2
    sus_states <- 1
    sus_inf_arr <- array(0, dim = c(2,2))
    do_AM <- TRUE
    do_keep_agent_data <- TRUE
    do_write_agent_data <- TRUE
    do_write_inits <- TRUE
    writing_dir <- "./tmp"

    if(!dir.exists(writing_dir)) dir.create(writing_dir)

    ## Jeez
    out <- write_inits(L = L,
                       ncores = ncores,
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
    expect_true(out)

    unlink(writing_dir, recursive = TRUE)
})



