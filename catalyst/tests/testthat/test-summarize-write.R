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


test_that("Write out output", {
    agent_status <- matrix(c(1, 1, 2,
                             2, 2, 2,
                             3, 2, 2,
                             3, 3, 3),
                           byrow = TRUE, ncol = 3)
    output_params_list <- list(do_write = FALSE,
                               save_sims = FALSE,
                               results_dir = NULL,
                               verbose = TRUE)
    ll <- 4
    env_status <- NULL
    do_AM <- FALSE
    disease_params_list <- list(K = 3, init_vals = c(3, 1, 0),
                                params = c(beta = 1, gamma = 1),
                                T = 3, infection_states = c(2),
                                CM_fxn = SIR_fxn)
    base_probs <- initialize_probs(disease_params_list,
                                   SIR_fxn)


    ###########################
    ll <- 1
    out_list1 <- run_cam_inner(ll, agent_status,
                              base_probs,
                              env_status,
                              neighbor_list,
                              output_params_list,
                              disease_params_list,
                              do_AM)
    ll <- 2 
    out_list2 <- run_cam_inner(ll, agent_status,
                              base_probs,
                              env_status,
                              neighbor_list,
                              output_params_list,
                              disease_params_list,
                              do_AM)
    cam_output <- list(out_list1, out_list2)

    ## Make the data frame from simulations
    df <- make_n_states_df(cam_output)
    ## Some tests for the resulting df
    expect_equal(as.numeric(table(df$ll)), c(4, 4))
    expect_true(all(df$tt %in% 0:3))
    expect_equal(dim(df), c(8, 5))

    ## Write everything
    output_params_list <- list(do_write = TRUE,
                               save_sims = FALSE,
                               results_dir = "~/temp_dir",
                               base_name = "my_sims",
                               verbose = TRUE)
    agent_list <- NULL
    env_list <- NULL
    sim_list <- list(L = 2)
    run_AM <- FALSE
    did_write <- write_output(cam_output,
                              output_params_list,
                              sim_list,
                              disease_params_list,
                              agent_list,
                              env_list,
                              run_AM)

    out_df <- read.csv(file.path(output_params_list$results_dir,
                                 paste0(output_params_list$base_name,
                                        "_nstates.csv")))
    expect_equal(df, out_df)
    input_params <- readRDS(file.path(output_params_list$results_dir,
                                 paste0(output_params_list$base_name,
                                        "_run_params.RDS")))
    expect_true(all(names(input_params) %in% c("output_params_list",
                                               "sim_list",
                                               "disease_params_list",
                                               "agent_list",
                                               "env_list",
                                               "run_AM")))
    unlink(output_params_list$results_dir, recursive = TRUE)
    
    
})
