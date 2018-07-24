context("Testing the AM step")

test_that("Testing get_agent_state_inds()",{
    infection_states <- c(2)
    susceptible_states <- c(1)
    current_agent_status <- c(3, 2, 2, 1, 1, 2)
    ind_list <- get_agent_state_inds(current_agent_status,
                                     infection_states,
                                     susceptible_states)
    expect_equal(ind_list$inf, c(2, 3, 6))
    expect_equal(ind_list$sus, c(4, 5))

})
          
