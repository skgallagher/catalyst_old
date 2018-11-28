context("Testing the non-interactive SI AM, where each susceptible agent has probability theta_n of becoming infected")



test_that("testing the sim", {

    ll <- 1
    agent_data <- matrix(0, ncol = 3, nrow = 5)
    theta <- 0
    T <- nrow(agent_data)
    N <- ncol(agent_data)
    agent_data[1, ] <- c(1, 1, 2) # initial values

    out <- non_interactive_si_am(ll, agent_data,
                                 theta, T, N)
    exp_out <- data.frame(S = rep(2, T),
                          I = rep(1, T),
                          ll = 1,
                          t = 1:T)
    expect_equal(out, exp_out)


})
