context("Test update functions")


test_that("List from get CM bounds is working", {
    kk <- 1
    ## Possible SIR model
    current_base_probs <- matrix(c(.25, .75, 0, 
                                   0, .8, .2,
                                   0, 0, 1),
                                 byrow = TRUE,
                                 nrow = 3)
    indexing_list <- list(c(1:5), c(6:7), c(8))
    ll <- get_CM_bounds_list(kk, current_base_probs,
                             indexing_list)
    expect_true(length(ll) == 2)
    expect_true(ll$N == 5)
    expect_equal(ll$bounds, cumsum(c(0, .25, .75, 0)))

    ## Make sure it works when there are zero counts
        kk <- 1
    ## Possible SIR model
    current_base_probs <- matrix(c(.25, .75, 0, 
                                   0, .8, .2,
                                   0, 0, 1),
                                 byrow = TRUE,
                                 nrow = 3)
    indexing_list <- list(NULL, c(6:7), c(8))
    ll <- get_CM_bounds_list(kk, current_base_probs,
                             indexing_list)
    expect_true(length(ll) == 2)
    expect_true(ll$N == 0)
    expect_equal(ll$bounds, cumsum(c(0, .25, .75, 0)))

})

test_that("agents are updating correctly", {
    ## FOR AM with very simple, deterministic transitions
    N <- 3
    K <- 3
    do_AM <- TRUE
    current_base_probs <- NULL
    current_agent_vec <- c(2, 1, 3)
    agent_probs <- matrix(c(0, 1, 0,
                            1, 0, 0,
                            0, 0, 1), byrow = TRUE,
                          nrow = 3)
    new_states <- update_agents(agent_probs,
                                current_agent_vec,
                                current_base_probs,
                                do_AM)
    expect_equal(new_states, current_agent_vec)
    ## Change forward
    current_agent_vec <- c(3, 3, 3)
    agent_probs <- matrix(c(0, 0, 1,
                            0, 0, 1,
                            0, 0, 1), byrow = TRUE,
                          nrow = 3)
    new_states <- update_agents(agent_probs,
                                current_agent_vec,
                                current_base_probs,
                                do_AM)
    expect_equal(new_states, current_agent_vec)

    ## FOR CM
    do_AM <- FALSE
    agent_probs <- NULL
    current_agent_vec <- c(3, 2, 2, 1)
    current_base_probs <- matrix(c(.25, .75, 0, ## Possible SIR
                                   0, .8, .2,
                                   0, 0, 1),
                                 byrow = TRUE,
                                 nrow = 3)
    ## Make sure indexing list is working as expected
    K <- 3
    new_indexing_list <- lapply(1:K, function(kk){
        inds <- which(current_agent_vec == kk)
        if(length(inds) > 0) return(inds)
        return(NULL)
    })
    expect_equal(new_indexing_list[[1]], c(4))
    expect_equal(new_indexing_list[[2]], c(2, 3))
    expect_equal(new_indexing_list[[3]], c(1))
    
    ## Have an empty category
    current_agent_vec <- c(2, 2, 3)
    new_indexing_list <- lapply(1:K, function(kk){
        inds <- which(current_agent_vec == kk)
        if(length(inds) > 0)   return(inds)
        return(NULL)
        
    })
    expect_true(is.null(new_indexing_list[[1]]))

    ## Draw some multinomials
    current_base_probs <- matrix(c(1, 0, 0, ## Possible SIR
                                   0, 1, 0,
                                   0, 0, 1),
                                 byrow = TRUE,
                                 nrow = 3)
    current_agent_vec <- c(rep(1, 3), rep(2, 2), rep(3, 1))
    ## Sort agents in terms of current status (at least store index)
    new_indexing_list <- lapply(1:K, function(kk){
        inds <- which(current_agent_vec == kk)
        if(length(inds) > 0)   return(inds)
        return(NULL)
        
    })
    agent_probs <- NULL
    do_AM <- FALSE
    new_states <- update_agents(agent_probs,
                                current_agent_vec,
                                current_base_probs,
                                do_AM)


     expect_equal(new_states, current_agent_vec)
    
})
