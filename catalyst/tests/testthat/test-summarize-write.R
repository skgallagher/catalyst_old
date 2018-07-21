context("Summarize and write functions")

test_that("Summarize a simulation", {
    agent_status <- matrix(c(1, 1, 2,
                             2, 2, 2,
                             3, 2, 2,
                             3, 3, 3),
                           byrow = TRUE, ncol = 3)
    output_params_list <- list(do_write = FALSE,
                               save_sims = FALSE,
                               results_dir = NULL,
                               verbose = FALSE)
    ll <- 4
    env_status <- NULL
    disease_params_list <- list(K = 3)
    do_AM <- FALSE

    out_list <- summarize_cam(agent_status,
                              output_params_list,
                              ll,
                              env_status,
                              disease_params_list,
                              do_AM)
    expect_true(out_list$ll == ll)
    expect_true(!out_list$do_AM)
    expected_states <- matrix(c(2, 1, 0,
                                0, 3, 0,
                                0, 2, 1,
                                0, 0, 3),
                              byrow = TRUE, nrow = 4,
                              ncol = 3)
    expect_equal(dim(out_list$n_states), dim(expected_states))
    expect_true(all((out_list$n_states - expected_states) == 0))

})
