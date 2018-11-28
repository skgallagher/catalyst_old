#' SI CM simulation
#'
#' @param init_vals vector (S0, I0)
#' @param theta beta, the infection param
#' @param T total number of time steps 0 to T inclusive
#' @return X_mat matrix of T+1 x 2 where entry ti is the number of individuals in state i at time t-1
SI_CM <- function(init_vals,
                  theta,
                  T){

    X_mat <- matrix(0, nrow = T+1, ncol = 2)
    X_mat[1, ] <- init_vals
    N <- sum(init_vals)
    for(tt in 2:(T+1)){
        p <- theta * (N - X_mat[tt-1, 1]) / N
        X_mat[tt, 1] <- X_mat[tt-1, 1] - rbinom(1, X_mat[tt-1, 1], p)
    }
    X_mat[, 2] <- N - X_mat[ ,1]
    return(X_mat)

}
                  
