context("Initialize functions")

test_that("Agents are correct", {

    init_CM_vals <- c(3, 1, 1)
    N <- sum(init_CM_vals)
    K <- 3
    T <- 3
    agents <- initialize_agents(init_CM_vals, N,
                                K, T)
    testthat::expect_true(all(agents[, 1] == c(1, 1, 1, 2, 3)))
    testthat::expect_equal(dim(agents), c(N, T+1))
    
    init_CM_vals <- c(0, 4, 1)
    N <- sum(init_CM_vals)
    K <- 3
    T <- 3
    agents <- initialize_agents(init_CM_vals, N,
                                K, T)
    testthat::expect_true(all(agents[, 1] == c(2, 2, 2, 2, 3)))
    

    }) 

test_that("Environments are correct", {

    tab <- table(c(1, 1, 1, 2, 2, 3))
    env_list <- list()
    env_list$init_env_table <- tab
    env_list$N <- sum(tab)
    env_list$E <- length(tab)
    env <- initialize_env(env_list, N, E)
    testthat::expect_equal(as.numeric(env), c(1, 1, 1, 2, 2, 3))


    tab <- table(list(a=c(1, 2, 2, 1, 1, 3),
                      b=c(1, 1, 1, 2, 2, 3)))
    env_list <- NULL
    env_list$init_env_table <- tab
    env_list$N <- sum(tab)
    env_list$E <- length(dim(tab)) #dim length
    env <- initialize_env(env_list, env_list$N, env_list$E)
    testthat::expect_equal(dim(env), c(env_list$N, env_list$E))
})


test_that("Initialize neighbor list is working", {

    ## Everyone is a neighbor
    tab <- table(c(1, 1, 1, 1, 1, 1))
    env_list <- list()
    env_list$init_env_table <- tab
    env_list$N <- sum(tab)
    env_list$E <- length(dim(tab))
    env_status <- initialize_env(env_list, N, E)
    nbrs <- initialize_neighbors(env_status, env_list$N,
                                 env_list$E)
    expect_true(all(sapply(nbrs, length) ==  5))
    
    ## Some mixing
    ## Person 3 by their lonesome
    tab <- table(list(a=c(1, 2, 2, 1, 1, 3),
                           b=c(1, 1, 1, 2, 2, 3)))
    env_list$init_env_table <- tab
    env_list$N <- 10
    env_list$E <- 2
    env <- initialize_env(env_list, env_list$N, env_list$E)


})
