context("the interactive AM")


test_that("catalyze_am_si_homog", {
    
    ll <-  1
    N <- 3
    T <- 10
    thetas <- c(.5, 0, 0)
    nbr_list <- rep(list(1:N), N)
    states <- 1:2
    inf_states <- 2
    sus_states <- 1
    agent_data <- matrix(0, ncol = N, nrow = T)
    agent_data[1, ] <- c(2, 1, 1)
    do_keep_agent_data <- FALSE
    do_write_agent_data <- FALSE
    writing_dir <- "tmp"

    out <- catalyze_am_si_homog(ll, thetas,
                                agent_data,
                                nbr_list,
                                states,
                                inf_states,
                                sus_states,
                                do_keep_agent_data,
                                do_write_agent_data,
                                writing_dir)

    expect_equal(out$X, matrix(rep(c(2,1), times = T), ncol =2, byrow=TRUE))
    
})

test_that("interactive_am_si_homog", {
    N <- 3
    thetas <- c(.5, 0, 0)
    nbr_list <- rep(list(1:N), N)
    states <- 1:2
    inf_states <- 2
    sus_states <- 1
    current_agent_data <- c(2,1,1)

    out <- interactive_am_si_homog(thetas,
                                   current_agent_data,
                                   nbr_list,
                                   states,
                                   inf_states,
                                   sus_states)
    expect_equal(out, c(2, 1, 1))
                                   
    


})


test_that("get_inf_nbrs",{
    
    N <- 3
    nbr_list <- rep(list(1:N), N)
    inf_inds <- 1
    out <- get_inf_nbrs(nbr_list, inf_inds)
    expect_equal(out, list(1, 1, 1))
    n_inf_nbrs <- sapply(out, length)
    expect_equal(n_inf_nbrs, c(1, 1, 1))

    ## no infectious
    inf_inds <- 0
    out <- get_inf_nbrs(nbr_list, inf_inds)
    expect_equal(out, list(NULL, NULL, NULL))

    n_inf_nbrs <- sapply(out, length)
    expect_equal(n_inf_nbrs, c(0, 0, 0))


    nbr_list <- list(c(1), c(2, 3), 

})
