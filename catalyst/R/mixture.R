## SKG
## January 2, 2019
## Mixture model yeah


#' Generate mixture model data for a SI with 2 groups
#'
#' @param w weight of number of agents in group 1
#' @param p1 prob of getting infected given agent is in group 1
#' @param p2  prob of getting infected given agent is in group 2
#' @param N total number of agents
#' @param T total number of time steps
#' @param x0 initial number of infectious
#' @return TxN matrix of agent data where entry t,n is state of agent n at time t
gen_mix_2_data <- function(w, p1, p2,
                           N, T,
                           x0){
    ## Get initial times
    mat <- matrix(0, nrow = T, ncol = N)
    mat[1, 1:x0] <- 1
    ## Latent variable Z
    Z <- rbinom(n = N, size = 1, prob = w) # 1 is group 1, 0 is group 2

    ## Loop through time steps and update
    for(tt in 1:(T-1)){
        sus_inds <- which(mat[tt,] == 0)
        inf_inds <- which(mat[tt,] == 1)
        mat[tt+1, sus_inds] <- ifelse(Z[sus_inds] == 1,
                                      rbinom(length(sus_inds), size = 1, prob = p1),
                                      rbinom(length(sus_inds), size = 1, prob = p2))
        mat[tt+1, inf_inds] <- 1


    }

       
    return(list(agent_data = mat, Z = Z))


}

#' Use EM alg. to get estimates of two group bernoulli mixture model
#'
#' @param theta0 vector of (w, p1, p2) - initial values
#' @param agent_data TxN matrix of agent data where entry t,n is state of agent n at time t
#' @param tol float - our convergence tolerance.  Default is .001
#' @param max_it integer - max number of iterations.  Default is 1000
#' @return list of best theta values (w, p1, p2) and conv_it iteration on which alg. converged or 0 if it did not
EM_mix_2 <- function(theta0, agent_data,
                     tol=1e-5, max_it = 1e3){
    theta <- theta0

    ## Subset agent_data to get rid of agents infected from beginning - they do not contribute to likelihood
    init_inf_inds <- which(agent_data[1,] == 1)
    agent_data <- agent_data[ ,-init_inf_inds]

    for(ii in 1:max_it){

        theta_new <- EM_calc_mix2(theta, agent_data)
        if(sqrt(sum((theta - theta_new)^2)) <= tol){
            return(list(theta = theta, conv_it = ii))
        }
        theta <- theta_new



    }

    return(list(theta = theta_new, conv_it = 0))

}

#' Do inner calculations for EM
#'
#' @param theta vector (w, p1, p2)
#' @param agent_data agent_data TxN matrix of agent data where entry t,n is state of agent n at time t
#' @return updated theta
#' @details see dissertation for calculations
EM_calc_mix2 <- function(theta, agent_data){
    T <- nrow(agent_data)
    theta_new <- theta
    w <- theta[1]
    p1 <- theta[2]
    p2 <- theta[3]
    sum_atn <- apply(agent_data[-T, , drop = FALSE],
                     2, function(col) sum(col == 0)) # vecof length N
    becomes_inf <- apply(agent_data, 2, function(col){
        if(any(col == 1)){
            return(1)
        } else {
            return(0)
        }
    })
    EZ_cond <- get_cond_exp_mix2(w, p1, p2, agent_data) # vectof of length N
    ## New values
    ## p1
    denom1 <- sum(EZ_cond * sum_atn)
    theta_new[2] <- sum(EZ_cond * becomes_inf) / denom1
    ## p2
    denom2 <- sum((1- EZ_cond) * sum_atn)
    theta_new[3] <- sum((1-EZ_cond) * becomes_inf) / denom2
    ## w
    theta_new[1] <- denom1 / (denom1 + denom2)
    return(theta_new)

    


}


#' Get the conditional expected value of Z
#'
#' @param w weight
#' @param w weight of number of agents in group 1
#' @param p1 prob of getting infected given agent is in group 1
#' @param p2  prob of getting infected given agent is in group 2
#' @param agent_data TxN matrix of agent data where entry t,n is state of agent n at time t
#' @return vector of size N
get_cond_exp_mix2 <- function(w, p1, p2, agent_data){

    N <- ncol(agent_data)
    T <- nrow(agent_data)
    Z <- rep(0, N)
    for(nn in 1:N){
        sus_inds <- which(agent_data[-T, nn] == 0)
        if(length(sus_inds) > 0){
            g1 <- w * prod(p1^(agent_data[sus_inds + 1, nn]) *
                           (1-p1)^(1-agent_data[sus_inds + 1, nn]))
            g2 <- (1-w) * prod(p2^(agent_data[sus_inds + 1, nn])  * 
                               (1-p2)^(1-agent_data[sus_inds + 1, nn]))
            Z[nn] <- g1 / (g1 + g2)## 
        }
    }
    return(Z)

}
