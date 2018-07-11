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
    ## Person 6 by their lonesome
    tab <- table(list(a=c(1, 2, 2, 1, 1, 3),
                           b=c(1, 1, 1, 2, 2, 3)))
    env_list$init_env_table <- tab
    env_list$N <- sum(env_list$init)
    env_list$E <- length(dim(env_list$init))
    env <- initialize_env(env_list, env_list$N, env_list$E)

    nbrs_list <- initialize_neighbors(env, env_list$N,
                                      env_list$E)
    actual_nbr_list <- list(c(2,3,4,5),
                            c(1, 3),
                            c(1, 2),
                            c(1, 5),
                            c(1, 4),
                            0)
    expect_true(all(mapply(all.equal, nbrs_list, actual_nbr_list)))

    ## Some 'null' assignments
    tab <- table(list(a=c(0, 0),
                           b=c(1, 2)))
    env_list$init_env_table <- tab
    env_list$N <- sum(env_list$init)
    env_list$E <- length(dim(env_list$init))
    env <- initialize_env(env_list, env_list$N, env_list$E)

    nbrs_list <- initialize_neighbors(env, env_list$N,
                                      env_list$E)
    actual_nbr_list <- list(0, 0)
    expect_true(all(mapply(all.equal, nbrs_list, actual_nbr_list)))

        

})


test_that("Initialize probabilities is working correctly", {
    params <- c(.1, .03)
    T <- 100
    init_vals <- c(950, 50, 0)
    CM_fxn = SIR_fxn
    disease_list <- list(params = params,
                         T = T,
                         init_vals = init_vals,
                         CM_fxn = CM_fxn)
    probs_mat <- initialize_probs(disease_list)
    expect_true(all(all.equal(dim(probs_mat), c(T, 3, 3))))
    ## All R -> R probs are 1
    expect_true(all(probs_mat[, 3, 3] == 1))
    ## All probs sum to 1
    expect_true(all(apply(probs_mat, 1:2, sum) == 1))
    ## Probs between 0 and 1
    expect_true(all(probs_mat >= 0))
    expect_true(all(probs_mat <= 1))
                
})
