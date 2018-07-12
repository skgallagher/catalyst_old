context("CM functions")
library(deSolve)

test_that("SIR works", {
    
    ## My favorite SIR
    params <- c(beta = .1, gamma = .03)
    T <- 100
    init_vals <- c(950, 50, 0)
    disease_list <- list(params = params,
                         T = T,
                         init_vals = init_vals)
    do_plot <- FALSE
    out <- integrate_CM(disease_list,
                        do_plot = do_plot)
    expect_true(TRUE)
        

})
