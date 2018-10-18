## SKG
## October 18, 2018
## Transmission functions


#' Scaffold for the SI transmission CM
#'
#' @param X number of agents in each step at time t for states 1:K
#' @param theta disease parameters.  Default is c(beta) for basic SI model
#' @return  a function that returns the KxK transition matrix where K is the number of states.  Entry ij is the POSTIVE number of agents moving from state i to state j from time t-1 to t for t=1, \dots, T.
SI <- function(X, theta){
    ## States are S=1, I = 2
    ## theta[1] = beta
    K <- length(X)
    N <- sum(X)
    mat <- matrix(0, nrow = K, ncol = K)
    mat[1, 2] <- theta[1] * X[1] * X[2] / N # S to I movement
    mat[1, 1] <- X[1] - mat[1, 2] # S to S movement
    mat[2, 2] <- X[2] # I to I movement
    return(mat)

}

#' Turn the difference/transmission matrix into matrix of probs
#
#' @param trans_fxn  a function that returns the KxK transition matrix where K is the number of states.  Entry ij is the POSTIVE number of agents moving from state i to state j from time t-1 to t for t=1, \dots, T.
#' @param X number of agents in each step at time t for states 1:K
#' @param theta disease parameters.  Default is c(beta) for basic SI model
#' @return a function that returns the KxK transition PROBABILITY matrix where K is the number of states.  Entry ij is the probability of an agent in state i moving to state j from time t-1 to t for t=1, \dots, T.
extract_prob_trans <- function(trans_fxn, X, theta){
    ## Each row ii of trans_fxn is divided by entry ii of X
    trans_fxn(X, theta) / X
}
