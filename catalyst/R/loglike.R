## SKG
## October 22, 2018
## Loglike

loglike_CM <- function(agent_data, theta,
                       D_fxn,
                       D_mat,
                       X_mat, K){

    N <- ncol(agent_data)
    T <- nrow(agent_data)

    for(tt in 2:T){
        for(kk in 1:K){
            Xk <- X_mat[tt, kk]
            Dk <- D_mat[tt-1, kk,] # from kk to others
            p <- theta_to_p(theta, X)
            
        }

    }
    return(loglike)
    


}


#' Compute loglike of the CM for the SIR model for many simulations of model
#'
#' @param theta vector of parameters (beta)
#' @param X_df a (L x (T+1))xK df where entry (l + t)i is the number of agents in state i at time t for simulation l
#' @param include_constant logical.  Default is FALSE.  If TRUE, we calculate the combination accounting for the possible permutaitons.  If FALSE, this constant is dropped.
#' @param do_interaction p is prob of infection from 1 - (1 - theta/N)^(#Inf at t-1).  Default is FALSE
#' @return single number, the negative log likelihood of theta given X_mat
loglike_CM_SI_many_sims <- function(theta, X_df, include_constant = TRUE,
                                    do_interaction = FALSE){
    neg_loglikes <- plyr::daply(.data = X_df, .var = c("ll "),
                                .fun = function(df){
                                    neg_loglike <- loglike_CM_SI(theta, df[, 1:2],
                                                                 include_constant,
                                                                 do_interaction)
                                    neg_loglike

                                })
    return(sum(neg_loglikes))

}


#' Compute loglike of the CM for the SIR model
#'
#' @param theta vector of parameters (beta)
#' @param X_mat a (T+1)xK matrix where entry ti is the number of agents in state i at time t
#' @param include_constant logical.  Default is FALSE.  If TRUE, we calculate the combination accounting for the possible permutaitons.  If FALSE, this constant is dropped.
#' @param do_interaction p is prob of infection from 1 - (1 - theta/N)^(#Inf at t-1).  Default is FALSE
#' @return single number, the negative log likelihood of theta given X_mat
loglike_CM_SI <- function(theta, X_mat, include_constant = TRUE,
                          do_interaction = FALSE){
    ## Constant in terms of data
    N <- sum(X_mat[1, ])
    loglike <- sum(sapply(2:(nrow(X_mat)), function(tt){
        ## ## If there are no susceptibles or no infeciouts, then we are done
        ## if(X_mat[tt-1, 1] == 0 | X_mat[tt-1, 2] == 0){
        ##     out <- 0
        ##     return(out)
        ## }
        p <- theta[1] * X_mat[tt-1, 2] / N
        if(do_interaction){
            p <- 1 - (1 - theta[1]/N)^X_mat[tt-1, 2]
        }
        delta <- X_mat[tt-1, 1] - X_mat[tt, 1]
        ckt <- 0
        if(include_constant){
            ckt <- log(choose(X_mat[tt - 1, 1], delta))
        }
        ## Stuff with the parameter theta
        pos <- sum(delta * log(p[1]))
        neg <- sum(X_mat[tt, 1] * log(1-p[1]))
        return(ckt + pos + neg)
    }))
 
    return(-loglike)
}


#' Compute loglike of the CM for the SI model
#'
#' @param theta vector of parameters
#' @param X_mat a (T+1)xK matrix where entry ti is the number of agents in state i at time t
#' @param include_constant logical.  Default is FALSE.  If TRUE, we calculate the combination accounting for the possible permutaitons.  If FALSE, this constant is dropped.
#' @return single number, the negative log likelihood of theta given X_mat
loglike_CM_SIR <- function(theta, X_mat, include_constant = TRUE){
    ## Constant in terms of data
    N <- sum(X_mat[1, ])
    loglike <- sapply(2:(nrow(X_mat)), function(tt){
        p <- theta[1] * X_mat[tt-1, 2] / N
        if(X_mat[tt-1, 1] <= 0 | X_mat[tt-1, 3] >= N |
           X_mat[tt-1, 2] <= 0) return(0)
        gamma <- theta[2]
        delta_S <- X_mat[tt-1, 1] - X_mat[tt, 1] # Change in S
        delta_R <- -(X_mat[tt-1, 3] - X_mat[tt, 3]) # Change in R
        ckt_S <- 0
        ckt_R <- 0
        if(include_constant){
            ckt_S <- log(choose(X_mat[tt - 1, 1], delta_S))
            ckt_R <- log(choose(X_mat[tt-1, 2], delta_R))
        }
        ## Stuff with the parameter theta
        log_S <- delta_S * log(p) + X_mat[tt, 1] * log(1-p)
        log_R <- delta_R * log(gamma) + X_mat[tt, 3] * log(1-gamma)
        out <- ckt_S + ckt_R + log_S + log_R
        if(any(is.nan(out))) browser()
        return(out)
    })


    return(-sum(loglike))
}


#' Calculate loglike for the AM SI framework
#'
#' @param theta vector of parameters
#' @param agent_data a T+1 x N matrix where T+1 is the final time step and N is the number of agents where entry tn = k means that agent n at time t is in state k.
#' @param nbr_list  of length N where entry n is a vector of indices of neighbors, that is people who share an environment assignment of non-zero for the same category.  The list entry has a 0 element if it has no neighbors
#' @return single number, the negative loglike of theta given the data
loglike_AM_SI <- function(theta, agent_data, nbr_list){
    T <- nrow(agent_data)
    loglike <- 0
    for(tt in 2:T){
        sus_inds <- which(agent_data[tt-1,] == 1)
        inf_inds <- which(agent_data[tt-1, ] == 2)
        for(nn in sus_inds){
            inf_nbr_inds <- find_inf_nbrs(inf_inds,
                                               nbr_list[[nn]])
            ## If there are no neighbors, then agent cannot be infected, so loglikelihood is 0
            if(all(inf_nbr_inds > 0)){
                n_inf_nbrs <- length(inf_nbr_inds)
                p <- 1 - (1 - theta[1]/ N)^n_inf_nbrs
                a <- agent_data[tt, nn] - 1
                loglike <- loglike + a * log(p) + (1-a) * log(1-p)
            }
        }
    }
    return(-loglike)

}

#' Get the indices of the infectious agents
#'
#' @param inf_inds indices of all the agents infected at time tt
#' @param nbr_inds indices of the neighbors of agent nn
#' @return indices of infectious neighbors of susceptible agent nn or 0 if there are no infectious neighbors
find_inf_nbrs <- function(inf_inds, 
                          nbr_inds){
    inf_nbr_inds <- intersect(inf_inds, nbr_inds)
    if(length(inf_nbr_inds) == 0) inf_nbr_inds <- 0
    return(inf_nbr_inds)


}



#' Return log like of bernoulli p
#'
#' @param params vector of parameters
#' @param agent_data a T+1 x N matrix where T+1 is the final time step and N is the number of agents where entry tn = k means that agent n at time t is in state k.
#' @param p_fxn How p goes to p_n
#' @param grouping_vec vector of size N of assignments of agents to groups
#' @param model_type either "group" or "mix2"
#' @param agent_data data frame or NULL.  Default is NULL
#' @param K number of total compartments
#' @return NEGATIVE loglike
#' @details see page on
loglike_p <- function(params, agent_data,
                      grouping_vec,
                      p_fxn = p_id,
                      model_type = "group",
                      K = NULL){
    param_list <- extract_params(params, model_type)
   
    N <- ncol(agent_data)
    T <- nrow(agent_data)
    loglike <- numeric(N)
    for(nn in 1:N){
        times <- which(agent_data[,nn] == 1) # which times are susceptible
        times <- times[times < T] # don't do last one
        for(tt in times){
            pn <- p_fxn(param_list$p, grouping_vec, param_list$weights,
                        agent_data, tt, K)
            loglike[nn] <- loglike[nn] +
                (agent_data[tt+1, nn] - 1) * log(pn[nn]) +
                (2 - agent_data[tt+1, nn]) * log(1-pn[nn])
        }
    }
    return(-sum(loglike))    

}

#' Extract the optimization parameters and sort into p and weights
#'
#' @param params vector of parameters
#' @param model_type either "group" or "mix2"
#' @return list of p, and weights
extract_params <- function(params, model_type){
    if(model_type == "group"){
        params_list <- list(p = params, weights = NULL)
        return(params_list)
    } else if(model_type == "mix2"){
        params_list <- list(p = params[1:2],
                            weights = params[-c(1:2)])
        return(params_list)
    }
    stop("This is not a valid parametrization")

}



#' Extract pn from p and group assignments
#'
#' @param p vector of size F with probs
#' @param grouping_vec vector of size N where entry i is equal to 1, .., F is the group assignment of agent i
#' @param agent_data data frame or NULL.  Default is NULL
#' @param tt time step.  Default is NULL
#' @param K number of total compartments
#' @return vector of size N
p_id <- function(p, grouping_vec, weights = NULL,
                 agent_data = NULL,
                 tt = NULL,
                 K = NULL){
    pn <- p[grouping_vec]
    return(pn)
}


#' Extract pn from p, the grouping vec, and weights
#'
#' @param p vector of size F with probs
#' @param grouping_vec vector of size N where entry i is equal to 1, .., F is the group assignment of agent i
#' @param agent_data data frame or NULL.  Default is NULL
#' @param tt time step.  Default is NULL
#' @param K number of total compartments
#' @return vector of size N where each entry is the probability
p_mix <- function(p, grouping_vec, weights,
                  agent_data = NULL,
                  tt = NULL, K = NULL){
    pn <- weights * p[1] + (1 - weights) * p[2]
    if(length(pn) < length(grouping_vec)){
        pn <- rep(pn[1], length(grouping_vec))
    }
    return(pn)

}


#' Extract pn from p, the grouping vec, and weights
#'
#' @param p vector of size F with probs
#' @param grouping_vec vector of size N where entry i is equal to 1, .., F is the group assignment of agent i
#' @param agent_data data frame or NULL.  Default is NULL
#' @param tt time step.  Default is NULL
#' @param K number of total compartments
#' @return vector of size N where each entry is the probability
#' @details This function does group interactive eg bk * #Inf/N
p_group_int <- function(p, grouping_vec,
                        weights, agent_data,
                        tt, K){
    X <- get_totals(agent_data, tt, K)
    pn <- p[grouping_vec]
    pn <- pn * X[2] / N # beta_n * I / N
    return(pn)

}
