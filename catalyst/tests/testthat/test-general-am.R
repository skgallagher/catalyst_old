context("AM pairwise interactions")


test_that("Testing update_susceptible_inds", {
    rolling_sus_inds <- 1:10
    infection_states <- c(2)
    sub_agent_probs <- matrix(c(
        0, 1, 0,
        1, 0, 0,
        .25, .75, 0,
        0, 1, 0
    ), byrow = TRUE, ncol = 3)
    susceptible_neighbors <- 1:4
    new_sus_inds <- update_susceptible_inds(rolling_sus_inds,
                                            sub_agent_probs,
                                            infection_states,
                                            susceptible_neighbors)
    expect_equal(new_sus_inds, c(2, 3, 5:10))

    ## Now when neighbors are actually indices c(1, 5, 8, 10)
    susceptible_neighbors <- c(1, 5, 8, 10)
    new_sus_inds <- update_susceptible_inds(rolling_sus_inds,
                                            sub_agent_probs,
                                            infection_states,
                                            susceptible_neighbors)
    expect_equal(new_sus_inds, c(2:9))

    ## No new infectious
    sub_agent_probs <- matrix(c(
        0, .25, .75,
        1, 0, 0,
        .25, .75, 0,
        0, 0, 1
    ), byrow = TRUE, ncol = 3)
    new_sus_inds <- update_susceptible_inds(rolling_sus_inds,
                                            sub_agent_probs,
                                            infection_states,
                                            susceptible_neighbors)
    expect_equal(new_sus_inds, rolling_sus_inds)

 
})


test_that("Testing the infect neighbors", {
    infector_state <- 2
    sub_agent_probs <- matrix(c(0, 1, 0,
                                1, 0, 0,
                                0, 1, 0,
                                0, 0, 1),
                              byrow = TRUE, ncol = 3)
    susceptible_neighbors <- 1:4
    ## Everyone stays where infector is, which should hopefully be state 2
    transmission_probs <- matrix(c(1, 0, 0,
                                   0, 1, 0,
                                   0, 0, 1),
                                 byrow = TRUE, ncol = 3)
    disease_params_list <- list(transmission_probs = transmission_probs,
                                contact_probs = 1)
    agent_vars <- NULL
    expected_new_probs <- matrix(c(0, 1, 0,
                                   0, 1, 0,
                                   0, 1, 0,
                                   0, 1, 0),
                                 byrow = TRUE, ncol = 3)
    new_probs <- infect_neighbors_draws(infector_state,
                                        sub_agent_probs,
                                        susceptible_neighbors,
                                        disease_params_list,
                                        agent_vars)
    expect_equal(new_probs, expected_new_probs)

})



test_that("Testing run_AM_step", {
    infector_state <- 2
    sub_agent_probs <- matrix(c(0, 1, 0,
                                1, 0, 0,
                                0, 1, 0,
                                0, 0, 1),
                              byrow = TRUE, ncol = 3)
    susceptible_neighbors <- 1:4
    ## Everyone stays where infector is, which should hopefully be state 2
    transmission_probs <- matrix(c(1, 0, 0,
                                   0, 1, 0,
                                   0, 0, 1),
                                 byrow = TRUE, ncol = 3)
    disease_params_list <- list(transmission_probs = transmission_probs,
                                contact_probs = 1)
    agent_vars <- NULL
    expected_new_probs <- matrix(c(0, 1, 0,
                                   0, 1, 0,
                                   0, 1, 0,
                                   0, 1, 0),
                                 byrow = TRUE, ncol = 3)
    new_probs <- infect_neighbors_draws(infector_state,
                                        sub_agent_probs,
                                        susceptible_neighbors,
                                        disease_params_list,
                                        agent_vars)
    expect_equal(new_probs, expected_new_probs)

})
