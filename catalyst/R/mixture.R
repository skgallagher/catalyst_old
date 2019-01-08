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
    if(x0 > 0){
        mat[1, 1:x0] <- 1
    }
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
#' @param theta_init vector of (w_vec, p_vec) - initial values
#' @param agent_data TxN matrix of agent data where entry t,n is state of agent n at time t
#' @param tol float - our convergence tolerance.  Default is .001
#' @param max_it integer - max number of iterations.  Default is 1000
#' @return list of best theta values (w, p1, p2) and conv_it iteration on which alg. converged or 0 if it did not
EM_mix_2 <- function(theta_init, agent_data,
                     tol=1e-3, max_it = 1e3){
    theta <- theta_init
    print(theta)
    ## Subset agent_data to get rid of agents infected from beginning - they do not contribute to likelihood for the SI model
    init_inf_inds <- which(agent_data[1,] == 1)
    if(length(init_inf_inds) > 0){
        agent_data <- agent_data[ ,-init_inf_inds]
    }

    for(ii in 1:max_it){

        theta_new <- EM_calc_mix2(theta, agent_data)
        print(theta_new)
        if(sqrt(sum((theta - theta_new)^2)) <= tol){
            return(list(theta = theta, conv_it = ii,
                        theta_init = theta_init))
        }
        theta <- theta_new


    }

    return(list(theta = theta_new, conv_it = 0,
                theta_init = theta_init))

}

#' Do inner calculations for EM
#'
#' @param theta vector (w, p_vec)
#' @param agent_data agent_data TxN matrix of agent data where entry t,n is state of agent n at time t
#' @return updated theta
#' @details see dissertation for calculations
EM_calc_mix2 <- function(theta, agent_data){
   
    K <- length(theta) / 2
    w_vec <- theta[1:K]
    p_vec <- theta[-c(1:K)]
    browser()

    ## E Step
    EZ_cond <- E_step_SI(w_vec, p_vec, agent_data)

    ## M Step
    new_theta <- M_step_SI(EZ_cond, agent_data)
    return(new_theta)
    


}

#' Do M step for SI mixture model
#'
#' @param EZ_cond expected value of Z conditioned on w and p_vec - a KxN matrix where entry k n is the expected value of Z for agent n.
#' @param  agent_data  agent_data TxN matrix of agent data where entry t,n is state of agent n at time t.  All agents are initially susceptible as the initially infected have been extracted
#' @return updated w_vec and updated p_vec combined into theta c(w_vec, p_vec)
M_step_SI <- function(EZ_cond, agent_data){
    browser()
    K <- nrow(EZ_cond)
    T <- nrow(agent_data)
    ## Get the number of days each agent was susceptible
    days_sus <- apply(agent_data[-T, , drop = FALSE], 2, function(col){
        sum(col == 0)
    })
    became_inf <- apply(agent_data, 2, function(col){
        sum(sum(col == 1) > 0) # at least one 1, which means agent became infected, zero otherwise
    })
    N_vec <- rowSums(EZ_cond * matrix(rep(days_sus, each = K), nrow = K)) # effective observations
    ## w
    w_vec <- N_vec / sum(N_vec)
    ##
    num <- rowSums(EZ_cond * matrix(rep(became_inf, each = K), nrow = K)) # numerator for updated p
    p_vec <- num / N_vec
    return(c(w_vec, p_vec))
    

}


#' Calculate the E Step for SI mixture model
#'
#' @param w vector of length K, weights for different mixtures
#' @param p_vec vector of length K, prob of becoming infected given individual is in group K
#' @param agent_data  agent_data TxN matrix of agent data where entry t,n is state of agent n at time t.  All agents are initially susceptible as the initially infected have been extracted
#' @return expected value of Z conditioned on w and p_vec - a KxN matrix where entry k n is the expected value of Z for agent n.  
E_step_SI <- function(w, p_vec, agent_data){

    K <- length(w)
    N <- ncol(agent_data)
    T <- nrow(agent_data)
    log_num_mat <- matrix(0, nrow = K, ncol = N)
    num_mat <- log_num_mat
    Z_mat <- num_mat
    for(kk in 1:K){
        for(nn in 1:N){
            log_num_mat[kk, nn] <-  log(w[kk])
            for(tt in 1:(T-1)){
                current_state <- agent_data[tt, nn]
                if(current_state == 1){
                    break
                }
                next_state <- agent_data[tt+1, nn]
                log_num_mat[kk, nn] <- log_num_mat[kk, nn] + next_state * log(p_vec[kk]) +
                    (1-next_state) * log(1 - p_vec[kk])
            }
            num_mat <- exp(log_num_mat)
        }
    }
    Z_mat <- num_mat / matrix(rep(colSums(num_mat), each = K), nrow = K)
    return(Z_mat)
}
