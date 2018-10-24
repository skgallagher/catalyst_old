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


#' Compute loglike of the CM for the SI model
#'
#' @param theta vector of parameters
#' @param X_mat a (T+1)xK matrix where entry ti is the number of agents in state i at time t
#' @return single number, the negative log likelihood of theta given X_mat
loglike_CM_SI <- function(theta, X_mat){
    ## Constant in terms of data
    N <- sum(X_mat[1, ])
    loglike <- sum(sapply(2:(nrow(X_mat)), function(tt){
        p <- theta * X_mat[tt-1, 2] / N
        delta <- X_mat[tt-1, 1] - X_mat[tt, 1]
        ckt <- log(choose(X_mat[tt - 1, 1], delta))
        ## Stuff with the parameter theta
        pos <- sum(delta * log(p[1]))
        neg <- sum(X_mat[tt, 1] * log(1-p[1]))
        return(ckt + pos + neg)
    }))
 
    return(-loglike)
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
