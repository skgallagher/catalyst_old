library(testthat)
devtools::load_all()

context("mixture models")

test_that("generating the data",{
    N <- 100
    T <- 8
    x0 <- 1

    w <- 1
    p1 <- 1
    p2 <- 1
    agent_data <- gen_mix_2_data(w, p1, p2,
                                 N, T,
                                 x0)$agent_data
    expect_true(all(unlist(agent_data[-1,]) == 1))

    
### EZ_step_SI
    N <- 10
    T <- 20
    x0 <- 0
    w <- .6
    p1 <- .1
    p2 <- .3
    ##
    agent_data_list <- gen_mix_2_data(w, p1, p2,
                                 N, T,
                                 x0)
    agent_data <- agent_data_list$agent_data
    Z <- agent_data_list$Z
    theta <- c(.5, .5, .4) #c(w, p_vec)


    p_vec <- c(p1, p2)
    w_vec <- c(w, 1 - w)
    EZ_cond <- E_step_SI(w_vec, p_vec, agent_data)
    expect_true(all(colSums(EZ_cond) == 1))


    ## M step following EZ
    out <- M_step_SI(EZ_cond, agent_data)


    ###########################################
    ## do itt
    N <- 1000
    T <- 20
    x0 <- 0n
    w <- .7
    p1 <- .1
    p2 <- .8
    K <-  2
    ##
    agent_data_list <- gen_mix_2_data(w, p1, p2,
                                 N, T,
                                 x0)
    agent_data <- agent_data_list$agent_data
    Z <- agent_data_list$Z
    

    p_vec <- c(p1, p2)
    w_vec <- c(w, 1 - w)
    r <- runif(2)
    theta_init <- c(rep(1/K, K), c(.3, .6))
    devtools::load_all()
    best_params <- EM_mix_2(theta_init, agent_data, max_it = 1000)
    best_params

})
