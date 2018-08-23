## SKG
## August 21, 2018


#' Wrapper for eval_difference_eqs, analogous to integrate_CM()
#'
#' @param disease_list List with
#' "params" - a numeric list of disease parameters (e.g. .1, .03)
#' "params_names" - optional variable names of params (e.g. beta, gamma)
#' "times" - max times we evaluate the diff eq
#' "init_vals" - vector of initial values of the states.  Must be nonnegative.  Length of vector is K, the number of states, and sum of values is N the constant number of individuals
#' @param CM_fxn - function to use in deSolve::ode or the difference equations
#' @param step - step size returned.  Default is 1.  It is set to NULL for difference equations
#' @param do_plot - Logical.  Default is FALSE.  If TRUE we return a base plot of the curves
#' @param ... plotting parameters to be passed
#' @return data frame of time and X1 through XK
sum_CM <- function(disease_list,
                   CM_fxn = SIR_diff,
                   step = NULL,
                   do_plot = FALSE,
                   ...){
    params <- disease_list$params
    init_vals <- disease_list$init_vals
    D <- CM_fxn
    times <- disease_list$times
    params_names <- disease_list$params_names
    out <- eval_difference_eqs(params = params,
                               init_vals = init_vals,
                               D = D,
                               times = times,
                               params_names = params_names,
                               do_plot = do_plot)
    return(out)

    


}


#' Evaluate the difference equations
#'
#' @param params vector of disease parameters (e.g. c(beta, gamma))
#' @param init_vals vector of initial values of states (e.g. (X0, Y0, Z0))
#' @param D a function describing the difference of moving from state i to state j from one time step to the next
#' @param times vector of times to reteurn.  Can be a subset of 0:T where T is the max value.  All integer values
#' @param params_names optional vector of names for the parameters.
#' @param do_plot logical.  Default is FALSE.  If TRUE, we return a base plot of the curves
#' @param ... plotting parameters to be bassed
#' @return data frame of times, and X1 through XK values at each time point
eval_difference_eqs <- function(params = c(.5, .25),
                                init_vals = c(950, 50, 0),
                                D = SIR_diff,
                                times = 0:30,
                                params_names = NULL,
                                do_plot = FALSE,
                                ...){

    times_sorted <- sort(times)
    T <- max(times_sorted)
    K <- length(init_vals)
    D_mat <- eval_D(params, init_vals, D, T)
    mat <- D_mat[which(0:T %in% times),]
    colnames(mat) <- paste0("X", 1:K)
    df <- data.frame(t = times, mat)

    ## Plot results
    if(do_plot){
        plot_ode_base(df, init_vals,
                      params, ...)
    }
    return(df)

   
}

#' Evaluate the difference equations to get actual values
#'
#' @param params
#' @param params vector of disease parameters (e.g. c(beta, gamma))
#' @param init_vals vector of initial values of states (e.g. (X0, Y0, Z0))
#' @param D a function describing the difference of moving from state i to state j from one time step to the next
#' @param T max time step
#' @return matrix of size (T+1) x length(init_vals) where entry tj is the number of agents/individuals in state j at time t-1
eval_D <- function(params, init_vals, D, T){
    K <- length(init_vals)
    X_mat <- matrix(0, ncol = K, nrow = (T+1))
    X_mat[1, ] <- init_vals
    for(tt in 1:T){
        X_mat[tt + 1, ] <- X_mat[tt, ] + D(params, X_mat[tt, ], tt)
    }
    return(X_mat)

}


#' The difference equation function for SIR
#'
#' @param params params vector of disease parameters (e.g. c(beta, gamma))
#' @param X vector of size K of state values. (e.g X1=950, X2=50, X3=0)
#' @param tt current time
#' @return diff_X vector of size K which gives us the difference in a particular compartment
SIR_diff <- function(params, X, tt){
    ## TODO: make generalizable
    N <- sum(X)
    K <- length(X)
    X_mat <- matrix(0, nrow = K, ncol = K)
    X_mat[1, 2] <- params[1] * X[1] * X[2] / N # Beta * S * I / N
    X_mat[2, 3] <- params[2] * X[2] # gamma I
    diff_X <- rowSums(t(X_mat) - X_mat)
    return(diff_X)


}


