## SKG
## Set up and testing helpers
## August 10, 2018

#' Make transmission probabilities
#'
#' @param beta rate of infection
#' @param N total number of agents
#' @param base_probs TxKxK matrix where entry tij is prob from moving from i to j from time t-1 to t
#' @return transmission matrix that is TxKxK where entry tij is probability of an agent in state i infecting a given agent to state j
make_transmission_probs_SIR <-function(beta, N,
                                       base_probs){
    p <- base_probs[, 1, 2]
    I <- p * N / beta
    prob_ind_trans <- 1-(1-p)^(1/I)
    transmission_probs <- base_probs
    transmission_probs[,,] <- 0
    transmission_probs[, 1, 1] <- 1
    transmission_probs[, 2, 1] <- 1 - prob_ind_trans
    transmission_probs[, 2, 2] <- prob_ind_trans
    transmission_probs[, 3, 3] <- 1
    return(transmission_probs)


}
