## SKG
## EM try 2
## Jan. 13, 2019


#' EM algorithm for the SI
#'
#' @param p1 probability of becoming infected given you are in group 1
#' @param p2 probability of becoming infected given you are in group 2
#' @param w probability of being in group 1
#' @param X last day of being susceptible
#' @param T last day observed
#' @param tol convergence tolerance.  Default is 1e-3
#' @param max_it max number of iterations
#' @return list of (p1, p2, w) estimates and number of iterations to converge or 0 if it did not
EM_SI <- function(p1, p2, w,
                  X, T,
                  tol = 1e-3,
                  max_it = 1e3){
    
    theta <- c(p1, p2, w)
    print(theta)

    for(ii in 1:max_it){
        ## E step
        EZ <- Estep_SI(theta, X, T)
        ## M step
        new_theta <- Mstep_SI(EZ, X, T)
        ## Check convergence
        if(all(abs(theta - new_theta) < tol)){
            return(list(best_theta = theta, conv_it = ii))
        }
        theta <- new_theta
        print(round(theta,8))

    }
    return(list(best_theta = c(p1, p2, p3), conv_it = 0))



}

#' E step of EM alg for SI
#'
#' @param theta c(p1, p2, w)
#' @param X last day of being susceptible (>= 1)
#' @param T last day of observations
#' @return conditional expected value of Z
Estep_SI <- function(theta, X, T){
    p1 <- theta[1]
    p2 <- theta[2]
    w <- theta[3]
    log_pz1 <- w * (X * log(1 - p1) + (X < T) * log(p1))
    pz1 <- exp(log_pz1)
    log_pz2 <- (1-w) * (X * log(1 - p2) + (X < T) * log(p2))
    pz2 <- exp(log_pz2)
    EZ <- pz1 / (pz1 + pz2)
    return(EZ)

}

#' M step for EM alg for SI
#'
#' @param EZ vector of length(X) which is the expected value of Z_n| w, p1, p2, X
#' @param X last day of being susceptible
#' @param T last day of observations
#' @return new c(p1, p2, w)
Mstep_SI <- function(EZ, X, T){

    ## p1
    denom1 <- sum(EZ * X) + sum(EZ * (X < T))
    p1 <- sum(EZ * (X < T)) / denom1
    ## p2
    denom2 <- sum((1-EZ) * X) + sum((1-EZ) * (X < T))
    p2 <- sum((1-EZ) * (X < T)) / denom2
    ## w
    w <- mean(EZ)
    return(c(p1, p2, w))

}
