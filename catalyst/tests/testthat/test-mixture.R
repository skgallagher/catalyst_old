library(testthat)
devtools::load_all("~/catalyst/catalyst")

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
    T <- 2
    x0 <- 0
    w <- .5
    p1 <- .1
    p2 <- .8
    ##
    agent_data_list <- gen_mix_2_data(w, p1, p2,
                                 N, T,
                                 x0)
    agent_data <- agent_data_list$agent_data
    Z <- agent_data_list$Z
    theta <- c(.5, .5, .4) #c(w, p_vec)


    devtools::load_all("~/catalyst/catalyst")
    p_vec <- c(p1, p2)
    w_vec <- c(w, 1 - w)
    EZ_cond <- E_step_SI(w_vec, p_vec, agent_data)
    expect_true(all(colSums(EZ_cond) == 1))


    ## M step following EZ
    out <- M_step_SI(EZ_cond, agent_data)


    ###########################################
    ## do itt
    N <- 10
    T <- 2
    x0 <- 0
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
    theta_actual <- c(w, 1-w, p1, p2)



    devtools::load_all("~/catalyst/catalyst")
    r <- runif(2)
    theta_init <- c(rep(1/K, K), r/ sum(r))
    theta_actual 
    best_params <- EM_mix_2(theta_init, agent_data, max_it = 1000)
    best_params

})

