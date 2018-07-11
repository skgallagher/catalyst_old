## Wrapper to make a general CM

#' Function using deSolve to numerically integrate the CM
#'
#' @param disease_list  List with
#' "params" - a numeric list of disease parameters (e.g. .1, .03)
#' "params_names" - optional variable names of params (e.g. beta, gamma)
#' "T" - max time.  The steps we take are 0, 1, ..., T inclusive
#' "init_vals" - vector of initial values of the states.  Must be nonnegative.  Length of vector is K, the number of states, and sum of values is N the constant number of individuals
#' @param CM_fxn - function to use in deSolve::ode
#' @param step - step size returned.  Default is 1
#' @param do_plot - Logical.  Default is FALSE.  If TRUE we return a base plot of the curves
#' @param ... plotting parameters to be passed
#' @return data frame of time and X1 through XK
integrate_CM <- function(disease_list, CM_fxn=SIR_fxn,
                         step = 1,
                         do_plot = FALSE,
                         ...){

    ## Extract input parameters
    params <- disease_list$params
    state <- disease_list$init_vals
    K <- length(state)
    T <- disease_list$T
    names(state) <- paste0("X", 1:K)
    times <- seq(0, T, by = step)

    ## Integrate with the solver
    ode_results <- deSolve::ode(state, times, func = SIR_fxn,
                                parms = params, maxsteps = 10000,
                                method = "rk4")

    ## Plot results
    if(do_plot){
        plot_ode_base(ode_results, state,
                      params, ...)
    }

    ## Return as data frame
    colnames(ode_results)[1] <- "t"
    results <- as.data.frame(ode_results)
    return(results)

   

}

#' Plot base R curves of results from deSolve:::ode
#'
#' @param ode_results - output from deSolve:::ode
#' @param state initial values.  Sum is N.  Length is K, the number of compartments
#' @param params disease parameters.
#' @param ... plotting params to be passed
#' @return TRUE
plot_ode_base <- function(ode_results, state, params, ...){
    ## Set up base plot
    N <- sum(state)
    K <- length(state)
    plot(1, 1, type = "n", xlim = range(ode_results[, 1]),
         ylim = range(ode_results[, 2:(K+1)]),
         main = paste0("CM curves \n N = ", N, "; Params = ",
                       paste0(params, collapse = ", ")))
         for(kk in 1:K){
             lines(ode_results[, 1], ode_results[, kk+1],
                   col = kk, lwd = 2)

         }
         legend("topright", paste0("X", 1:K),
                col = 1:K, lwd =2)
                       
    return(TRUE)

}


#' Set up a CM based on inputs
#'
#' @param disease_list  List with
#' "params" - a numeric list of disease parameters (e.g. .1, .03)
#' "params_names" - optional variable names of params (e.g. beta, gamma)
#' "T" - max time.  The steps we take are 0, 1, ..., T inclusive
#' "init_vals" - vector of initial values of the states.  Must be nonnegative.  Length of vector is K, the number of states, and sum of values is N the constant number of individuals
#' @return a function to use in ode45 from diffeq package
#' @details Currently hardcoded for SIR only
make_CM_fxn <- function(disease_list, N, K){
    CM_fxn <- SIR_fxn
    return(CM_fxn)

}


SIR_fxn <- function(t, state, params){
    N <- sum(state)
    with(as.list(c(state, params)), {
        dX1 <- -params[1] * state[1] * state[2] / N
        dX3 <- params[2] * state[2]
        dX2 <- -dX1 - dX3
        list(c(dX1, dX2, dX3))
    })
}
