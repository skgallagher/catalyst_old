testthat::context("testing SIR simulations")


test_that("colVar",{
    mat <- matrix(c(0, 1, 1,
                    0, 2, 2,
                    0, 3, 9), byrow = TRUE, nrow = 3)
    exp <- c(var(mat[,1]), var(mat[,2]), var(mat[,3]))
    out <- colVar(mat)
    expect_equal(exp, out)
})

test_that("sir_bin_km",{

    params <- c("beta" = .1, "gamma" = .03)
    T <- 50
    K <- 3
    X0 <- c(950, 50, 0)
    out <- sir_bin_km(params, T, K, X0)
    expect_equal(dim(out), c(T, 3))

})


test_that("simulate_SIR",{
    params <- c("beta" = .1, "gamma" = .12, "rho" = .002)
    T <- 55
    K <- 3
    L <- 100
    X0 <- c(187, 1, 0)
    N <- sum(X0)
    mod_fxn <- sir_bin
    prob_fxn <- rf_prob
    out <- simulate_SIR(params, mod_fxn = mod_fxn,
                        prob_fxn = prob_fxn,
                        T = T, L = L, K = 3,
                        X0 = X0)

    df_ave <- out$df_ave
    df_var <- out$df_var
    library(ggplot2)
    library(reshape2)
    df_am <- melt(df_ave, id.vars = c("t", "type"))
    df_vm <- melt(df_var, id.vars = c("t", "type"))
    df <- data.frame(df_am, lower = (df_am$value - 2 * sqrt(df_vm$value))/N * 100,
                     upper = (df_am$value + 2 * sqrt(df_vm$value)) / N * 100)
    df$value <- df$value / N * 100
    
    ggplot(data = df, aes( x = t, col = variable, fill = variable, group = variable)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2, col = NA) +
        geom_line(aes(y = value), size = 1.5) +
        scale_colour_manual(values = c("blue", "red", "yellow3")) +
        scale_fill_manual(values = c("blue", "red", "yellow3"))

    ## Plot sims of I
    df_I <- data.frame(out$I_mat)
    df_I$t <- 1:nrow(df_I)
    df_Im <- melt(df_I, id.vars = "t")
    ggplot(df_Im, aes(x = t)) + geom_line(aes(y = value, group = as.integer(variable),
                                              col = as.integer(variable)))
                                          
        

})

test_that("loglike_sir_bin", {
    ## Good luck!!
    params <- c("beta" = .1, "gamma" = .03, "rho" = .002)
    T <- 100
    K <- 3
    L <- 100
    X0 <- c(950, 50, 0)
    N <- sum(X0)
    mod_fxn <- sir_bin
    prob_fxn <- rf_prob
    out <- simulate_SIR(params, mod_fxn = mod_fxn,
                        prob_fxn = prob_fxn,
                        T = T, L = L, K = 3,
                        X0 = X0)

}

test_that("am_sir_sims", {
    params <- c("beta" = 1, "gamma" = .2)
    N <- 5
    T <- 4
    K <- 3
    L <- 1
    A <- matrix(0, nrow = T, ncol = N)
    A[1,] <- c(1, 1, 1, 1, 2)
    permute_inds <- TRUE
    out <- am_sir_sims(L = L,params, A, T, N, K,
                       agent_update_fxn = update_agent_sir,
                       prob_fxn = km_prob,
                       permute_inds = permute_inds)
    ## devtools::load_all()
#    expect_equal(dim(out), c(T, N))
    ##
    permute_inds <- FALSE
    params <- c("beta" = 1, "gamma" = 1)
    out <- am_sir_sims(L = 1,params, A, T, N, K,
                       agent_update_fxn = update_agent_sir,
                       prob_fxn = km_prob,
                       permute_inds = permute_inds)


})

test_that("get_state_inds", {
    N <- 5
    T <- 1
    
    mat <- matrix(c(1, 2, 2, 1, 3), nrow = T, ncol = N)
    K <- 2
    out <- get_state_inds(mat[1,], K)
    exp_list <- list(c(1, 4), c(2, 3))
    expect_equal(exp_list, out)
    ##
    K <- 3
    out <- get_state_inds(mat[1,], K)
    exp_list <- list(c(1, 4), c(2, 3), c(5))
    expect_equal(exp_list, out)
    ## Missing some
    mat <- matrix(c(1, 3, 1, 1, 3), nrow = T, ncol = N)
    K <- 3
    out <- get_state_inds(mat[1,], K)
    exp_list <- list(c(1, 3, 4), NA, c(2, 5))
    expect_equal(exp_list, out)
    

    })

test_that("update_agent_sir", {
    params <- c("beta" = 1, "gamma" = 1)
    N <- 5
    K <- 3
    prob_fxn <- km_prob
    permute_inds <- TRUE
    vec <- c(1, 1, 2, 2, 3)
    kk <- 1
    state_inds_list <- get_state_inds(vec, K)
    new_states <- update_agent_sir(params, kk, N,
                                   state_inds_list,
                                   prob_fxn, permute_inds)    
    expect_equal(length(new_states), 2)
    ##
    kk <- 2
    state_inds_list <- get_state_inds(vec, K)
    new_states <- update_agent_sir(params, kk, N,
                                   state_inds_list,
                                   prob_fxn, permute_inds)
    expect_equal(length(new_states), 2)
    ##
    kk <- 3
    new_states <- update_agent_sir(params, kk, N,
                                   state_inds_list,
                                   prob_fxn, permute_inds)
    expect_equal(new_states, 3)
    ## Try when  permute_inds = FALSE
    permute_inds <- FALSE
    kk <- 2
    params <- c("beta" = 1, "gamma" = .5)
    new_states <- update_agent_sir(params, kk, N,
                                   state_inds_list,
                                   prob_fxn, permute_inds)
    
    
   

    
})


## library(testthat)
##    devtools::load_all()
