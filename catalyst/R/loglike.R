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
#' @return single number, the log likelihood of theta given X_mat
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

