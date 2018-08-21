context("Difference equation functions")


test_that("SIR difference equations work", {
    
    ## My favorite SIR
    params <- c(beta = .1, gamma = .03)
    T <- 100
    init_vals <- c(950, 50, 0)
    disease_list <- list(params = params,
                         T = T,
                         init_vals = init_vals)
    do_plot <- TRUE


    ## SIR_diff
    out <- SIR_diff(params, init_vals, tt)
    N <- sum(init_vals)
    change <- numeric(3)
    change[1] <- -params[1] * init_vals[1] * init_vals[2] / N
    change[3] <- params[2] * init_vals[2]
    change[2] <- -(change[1] + change[3])
    x_out <- init_vals  + change
    expect_equal(out, change)

    ## eval_D
    T <- 100
    D <- SIR_diff
    out <- eval_D(params, init_vals, D, T)
    expect_equal(dim(out), c(T+1, length(init_vals)))

    ## eval_difference_eqs
    out <- eval_difference_eqs(params = c(.1, .03),
                               init_vals = c(950, 50, 0),
                               D = SIR_diff,
                               times = 0:100,
                               params_names = NULL,
                               do_plot = FALSE)
    expect_equal(dim(out), c(T+1, length(init_vals) + 1))
    
        

})
