## SKG
## MLE and LSE of beta/gamma for SIR


#' Format data to add differences between states from one time to the next
#' 
#' @param data data frame of OBSERVED time, X1, X2, and X3 values
#' @return new_data with differences and lagged values at each time step
get_SIR_diffs <- function(data){
    ## Return X1, X2, X3 diffs and previous values
    sub_df <- plyr::ddply(.data = data, .variables = c("ll"),
                          .fun =  function(x){
                              obs_X1 <- c(dplyr::lag(x$X1, 1))
                              obs_X2 <- c(dplyr::lag(x$X2, 1))
                              obs_X3 <- c(dplyr::lag(x$X3, 1))
                              return(data.frame(obs_X1= obs_X1,
                                                obs_X2 = obs_X2,
                                                obs_X3 = obs_X3))
                          })
    sub_df$diff_X1 <- sub_df$obs_X1 - data$X1
    sub_df$diff_X3 <- -sub_df$obs_X3 + data$X3
    return(data.frame(data, sub_df[, -1]))

}
                          

#' Format SIR data to use in likelihood
#'
#' @param params beta and gamma
#' @param new_data SIR data with obs_X1, obs_X2, and obs_X3 values, which are lagged differences
#' @param disease_list list with parameters
#' "params" - a numeric list of disease parameters (e.g. .1, .03)
#' "params_names" - optional variable names of params (e.g. beta, gamma)
#' "T" - max time.  The steps we take are 0, 1, ..., T inclusive
#' "times" - all times
#' "init_vals" - vector of initial values of the states.  Must be nonnegative.  Length of vector is K, the number of states, and sum of values is N the constant number of individuals
#' @param do_plug_in whether to use theoretical or observed SIR values in probability of transition in likelihood
get_SIR_lags <- function(params,
                         new_data,
                         disease_list,
                         do_plug_in = TRUE){
    if(!do_plug_in){
        ## USE theoretical SIR values in like
        disease_list$params <- params
        ode_results <- sum_CM(disease_list, CM_fxn = SIR_diff, # hard coded
                                    step = 1, do_plot = FALSE)
        ode_results <- as.data.frame(ode_results)
        ode_results$tt <- ode_results$t + 1
        colnames(ode_results) <- c("old_t", "lag_X1", "lag_X2", "lag_X3", "tt")
        new_data <- plyr::join(new_data, ode_results[,-1], by = "tt")

    } else {
        ## DON'T RUN the actual SIR   
        new_data$lag_X1 <- new_data$obs_X1
        new_data$lag_X2 <- new_data$obs_X2
        new_data$lag_X3 <- new_data$obs_X3

    }
    return(new_data)

}


#' Estimate negative log like from SIR
#'
#' @param params vector of parameters (e.g. beta, gamma)
#' @param data data frame of OBSERVED time, X1, X2, and X3 values
#' @param disease_list list with parameters
#' "params" - a numeric list of disease parameters (e.g. .1, .03)
#' "params_names" - optional variable names of params (e.g. beta, gamma)
#' "T" - max time.  The steps we take are 0, 1, ..., T inclusive
#' "init_vals" - vector of initial values of the states.  Must be nonnegative.  Length of vector is K, the number of states, and sum of values is N the constant number of individuals
#' @param do_plug_in whether to use theoretical or observed SIR values in probability of transition in likelihood
#' @return negative log likelihood of SIR given the data
loglike_sir <- function(params,
                        data,
                        disease_list = NULL,
                        do_plug_in = FALSE){


    N <- sum(disease_list$init_vals)
    beta <- params[1]
    gamma <- params[2]

    new_data <- get_SIR_diffs(data)
    new_data <- get_SIR_lags(params, new_data, disease_list, do_plug_in)
   

    new_data <- na.omit(new_data)

    
    s_loglike <- sum(apply(new_data, 1,
                           function(row){
                               prob <- beta * row["lag_X2"] / N
                               if(prob == 0){
                                  return(-10e3)
                               }
                               like <- dbinom(row["diff_X1"],
                                          size = row["obs_X1"],
                                          prob = prob)
                               if(like > 0) return(log(like))
                               return(-10e3)
                               
                            }))
    r_loglike <- sum(apply(new_data, 1,
                           function(row){
                               if((N - row["obs_X1"] - row["obs_X3"]) == 0) return(0)
                               like <- dbinom(row["diff_X3"],
                                          size = (N - row["obs_X1"] - row["obs_X3"]),
                                          prob = gamma)
                               if(like > 0) return(log(like))
                               return(-10e3)
                            }))
    loglike <- s_loglike + r_loglike
    return(-loglike)
}

