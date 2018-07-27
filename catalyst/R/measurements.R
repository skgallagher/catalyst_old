## SKG
## MLE and LSE of beta/gamma for SIR


get_SIR_diffs <- function(data){
    ## Return X1, X2, X3 diffs and previous values
    sub_df <- plyr::ddply(.data = data, .variables = c("ll"),
                          .fun =  function(x){
                              obs_X1 <- c(lag(x$X1, 1))
                              obs_X2 <- c(lag(x$X2, 1))
                              obs_X3 <- c(lag(x$X3, 1))
                              return(data.frame(obs_X1= obs_X1,
                                                obs_X2 = obs_X2,
                                                obs_X3 = obs_X3))
                          })
    sub_df$diff_X1 <- sub_df$obs_X1 - data$X1
    sub_df$diff_X3 <- -sub_df$obs_X3 + data$X3
    return(data.frame(data, sub_df[, -1]))

}
                          

get_SIR_lags <- function(params,
                         new_data,
                         disease_list,
                         do_plug_in = TRUE){
    if(!do_plug_in){
        ## USE observed SIR values in like

        ode_results <- integrate_CM(disease_list, CM_fxn = SIR_fxn,
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

