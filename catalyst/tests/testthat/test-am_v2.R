context("Agent interactions for v2")


test_that("rescale agent prob",{
    agent_prob <- c(.25, 0, .75)
    zero_prob_states <- 2
    new_prob <- rescale_agent_prob(agent_prob, zero_prob_states)
    expect_equal(agent_prob, new_prob)

    ## Test 2
    agent_prob <- c(.25, .25, .5)
    zero_prob_states <- 2
    new_prob <- rescale_agent_prob(agent_prob, zero_prob_states)
    expect_equal(c(.25, 0, .5) / .75, new_prob)
    


})

test_that("extract_transition_states", {

    ## SI
    sus_state <- 1
    inf_states <- 2
    sus_inf_arr <- array(0, dim = c(2, 2, 2))
    sus_inf_arr[1, 2, 2] <- 1 # sus can be put in I by an infectious
    z <- extract_transition_states(1, 2, sus_inf_arr)
    expect_equal(z, 2)

    ## SEIR
    sus_state <- 1
    inf_states <- 3
    sus_inf_arr <- array(0, dim = rep(4, 3))
    sus_inf_arr[1, 3, 2] <- 1 # sus can be put into E by infectious I
    z <- extract_transition_states(sus_state, inf_states, sus_inf_arr)
    expect_equal(z, 2)

    ## S^2IR
    sus_state <- 1
    inf_states <- 3
    sus_inf_arr <- array(0, dim = rep(4, 3))
    sus_inf_arr[1, 3, 3] <- 1 # sus 1 can be put into I
    sus_inf_arr[2, 3, 3] <- 1 # sus 1 can be put into I
    z <- extract_transition_states(sus_state, inf_states, sus_inf_arr)
    expect_equal(z, 3)

    ## SI^2
    sus_state <- 1
    inf_states <- 2:3
    sus_inf_arr <- array(0, dim = rep(4, 3))
    sus_inf_arr[1, 2, 2:3] <- 1 # sus 1 can be put into I1 or I2
    sus_inf_arr[2, 3, 2:3] <- 1 # sus 1 can be put into I1 or I2
    z <- extract_transition_states(sus_state, inf_states, sus_inf_arr)
    expect_equal(z, 2:3)

})


test_that("infect_nbr", {
    ## SI
    sus_state <- 1
    inf_state <- 2
    sus_inf_arr <- array(0, dim = rep(2, 3))
    sus_inf_arr[1, 2, 2] <- 1 # absolute infection
    z <- infect_nbr(sus_state, inf_state, sus_inf_arr)
    expect_equal(z, 2)
    sus_inf_arr[1, 2, 2] <- 0 # absolute non-infection
    z <- infect_nbr(sus_state, inf_state, sus_inf_arr)
    expect_equal(z, 0)

})


test_that("interact_agents", {

    ## SI
    inf_indices <- 2
    sus_indices <- 1
    inf_states <- 2
    T <- 3
    N <- length(c(inf_indices, sus_indices))
    tt <- 1
    agent_data <- matrix(1, nrow = T, ncol = N)
    agent_data[1, 2] <- 2 # agent 2 is infected
    nbr_list <- list(c(2), 1)  # They're neighbors
    ## first originally not to be infected
    agent_probs <- matrix(c(.5, .5, 0, 1),
                          byrow = TRUE, ncol = 2)
    sus_inf_arr <- array(0, dim = rep(2, 3))
    sus_inf_arr[1, 2, 2] <- 1 # absolute infection

    ## Testing
    z <- interact_agents(inf_indices, sus_indices, inf_states,
                    agent_data,
                    nbr_list,
                    tt,
                    sus_inf_arr,
                    agent_probs)
    expect_equal(z[1,2], 1)
    

    ## Next test: multiple agents
                          
                          
                          

})
