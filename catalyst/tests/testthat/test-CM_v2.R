context("True CM model, v fast")

test_that("SI_CM", {
    init_vals <- c(9, 1)
    theta <- 0
    T <- 9
    out <- SI_CM(init_vals, theta, T)
    exp_out <- matrix(rep(init_vals, times = T+1),
                      byrow = TRUE)

}
