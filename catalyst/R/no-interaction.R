## Functions for making the CM/AM with probability of transition as theta_n, e.g. the non interactive version


#' CM/AM for the SI with probability of transition theta_n
#'
#' @param ll simulation number
#' @param agent_data a T x N matrix where T+1 is the final time step and N is the number of agents where entry tn = k means that agent n at time t is in state k.
#' @param theta either a single value or a vector of size N where the value is the probability of becoming infectious for a susceptible agent from one time step to the next
#' @param T total number of time steps, nrow(agent_data)
#' @param N total number of agents
#' @return X a dataframe that is T x 4 where entry ti is the number of individuals in state i=1,2 at time t.  Column 3 is the simulation number and column 4 is the time
#' @details CM: A_{t,n}| A_{t-1,n} ~ Bernoulli(theta) + 1
#' AM: A_{t,n}| A_{t-1,n} ~ Bernoulli(theta_n) + 1
non_interactive_si_am <- function(ll, agent_data, theta, T, N){

    if(length(theta) == 1){
        theta <- rep(theta, N)
    }
    mat <- matrix(0, nrow = T, ncol = 4)
    X <- as.data.frame(mat)
    colnames(X) <- c("S", "I", "ll", "t")
    X$ll <- ll
    X$t <- 1:T
    for(tt in 1:T){

        if(tt < T){ # don't need one for time T
            agent_data[tt+1, ] <- sapply(1:N, function(nn){
                out <- agent_data[tt, nn]
                if(agent_data[tt, nn] == 1){
                    out <- rbinom(n=1, size = 1, prob = theta[nn]) + 1
                }
                return(out)
            
            })
        }
        
        X[tt, c("S", "I")] <- get_totals(agent_data, tt, K = 2)
    }

    return(X)


    }


#' Calculate expected value and variance for simulation for given values
#'
#' @param init_sus_inds indices of initially infectious individuals
#' @param T total number of steps
#' @param theta either a single value or a vector of size N where the value is the probability of becoming infectious for a susceptible agent from one time step to the next
#' @return X a dataframe that is T x 2 where entry ti is the number of individuals in state i=1,2 at time t. 
EV_si <- function(init_sus_inds, T, theta){
    if(length(theta) == 1) theta <- rep(theta, length(init_sus_inds))
        
    x0 <- length(init_sus_inds)
    X <- data.frame(X1_mean = rep(x0, T), X2_mean = rep(N - x0, T),
                    X1_var = rep(0, T), X2_var = rep(0, T))

    if(x0 < 1){
        return(X)
    }
    for(tt in 2:T){
        sus_thetas <- theta[init_sus_inds]
        X$X1_mean[tt] <- sum((1 - sus_thetas)^(tt-1))
        X$X1_var[tt] <- sum(
        (1 - sus_thetas)^(2*(tt-1)) *
        (1 - (1 - sus_thetas)^(tt-1)) +
        (1 - (1 - sus_thetas)^(tt-1))^2 *
        (1 - sus_thetas)^(tt-1)
                         )
    }
    X$X2_mean <- N - X$X1_mean
    X$X2_var <- X$X1_var
    return(X)

}
