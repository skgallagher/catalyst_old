
context("Random draws")


test_that("Multinomial draws -- AM", {
    ## TODO
    ## RUN SOME TESTS
    agent_probs <- matrix(rep(c(.25, .75)), 5, ncol = 2, byrow = TRUE)
    
    ## Basic Tests
    new_states <- draw_multinom(agent_probs,
                                bounds = NULL,
                                N = nrow(agent_probs))
    expect_true(all(new_states %in% c(1,2)))

    ## When prob is 1, should always go to that state
    agent_probs <- matrix(c(1, 0), ncol = 2, byrow = TRUE)
    B <- 10000
    results <- matrix(0, ncol = 1, nrow =B)
    for(bb in 1:B){
        results[bb,] <- draw_multinom(agent_probs)
    }
    expect_true(all(dim(results) == c(B, 1)))
    expect_true(all(results == 1))

    ## When prob is 1, should always go to that state, now for other compartment
    agent_probs <- matrix(c(0, 1), ncol = 2, byrow = TRUE)
    B <- 10000
    results <- matrix(0, ncol = 1, nrow =B)
    for(bb in 1:B){
        results[bb,] <- draw_multinom(agent_probs)
    }
    expect_true(all(dim(results) == c(B, 1)))
    expect_true(all(results == 2))

    ## Try a more complicated multinomial and hope ANOVA doesnt go wacky
    K <- 5
    base_probs <- c(.1, .2, .25, .15, .3)
    agent_probs <- matrix(base_probs, nrow = 1,
                          ncol = K, byrow = TRUE)
    results <- matrix(0, ncol = 1, nrow =B)
    for(bb in 1:B){
        results[bb,] <- draw_multinom(agent_probs)
    }
    empirical_probs <- table(results) / B
    expect_true(length(empirical_probs) == K)
    chisq_results <- chisq.test(x = as.numeric(table(results)),
                                p =   base_probs)
    expect_true(chisq_results$p.value > .05)
    
})


test_that("Multinomial Draws -- CM", {
    agent_probs <- NULL
    bounds <- c(0, 0, 1)
    N <- 5
    draws <- draw_multinom(agent_probs, bounds, N)
    expect_true(all( draws == 2))
    expect_true(length(draws) == 5)
    
})
