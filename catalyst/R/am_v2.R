##  AM version 2 functions
## October 10, 2018
## SKG


#' Perform the interaction/infection step
#'
#' @param inf_indices indices of the currently infected agents
#' @param sus_indices indices of currently susceptible agents
#' @param inf_states vector of infectious state categories
#' @param agent_data a T x N matrix where T+1 is the final time step and N is the number of agents where entry tn = k means that agent n at time t is in state k.
#' @param nbr_list of length N where entry n is a vector of indices of neighbors, that is people who share an environment assignment of non-zero for the same category.  The list entry has a 0 element if it has no neighbors
#' @param tt current time step
#' @param sus_inf_arr a K x K x K array where entry ijk=1 means that an interaction between an agent in susceptible state i can put an agent in infectious state j into state k with non-zero probability.
#' @param agent_probs a N x K which is the prob of the agent transitioning to state k conditioned on their current state
#' @return Updated agent_probs
interact_agents <- function(inf_indices,
                            sus_indices,
                            inf_states,
                            agent_data,
                            nbr_list,
                            tt,
                            sus_inf_arr,
                            agent_probs){

    ## Loop over susceptibles
    for(ss in sus_indices){
        ## Extract the infectious neighbor indices
        inf_nbr_inds <- intersect(nbr_list[[ss]], inf_indices)
        is_infected <- FALSE
        if(length(inf_nbr_inds) > 0){
            for(ii in inf_nbr_inds){
                inf_state <- infect_nbr(agent_data[tt, ss], agent_data[tt, ii],
                                        sus_inf_arr)
                if(inf_state > 0){ # if greater than 0 then ss has been infected
                    agent_probs[ss,] <- 0  ## All other probs become 0
                    agent_probs[ss, inf_state] <- 1 ## Change of infection is 1
                    is_infected <- TRUE
                    break # first infection wins
                }
            }

        }
        if(!is_infected){ # If not infected, rescale probabilities
            zero_prob_states <- extract_transition_states(agent_data[tt, ss],
                                                          inf_states,
                                                          sus_inf_arr)
            agent_probs[ss,] <- rescale_agent_prob(agent_probs[ss,],
                                                   zero_prob_states)
        }
    }


    return(agent_probs)

}

#' Infect your susceptible neighbor
#'
#' @param sus_state the integer of the susceptible state that the susceptible agent is in
#' @param inf_state the integer of the infectious state the infectious agent is in
#' @param sus_inf_arr a K x K x K array where entry ijk=1 means that an interaction between an agent in susceptible state i can put an agent in infectious state j into state k with non-zero probability.
#' @return an integer between 0 and K.  0 if the nbr has escaped infection and K which corresponds to the state the agent is going to move to
infect_nbr <- function(sus_state, inf_state,
                       sus_inf_arr){

    ## TODO
    ## This is not going to be efficient...
    prob_move <- sus_inf_arr[sus_state, inf_state, ]
    move_states <- which(prob_move > 0)
    multi_draw <- draw_multinom(matrix(prob_move, nrow = 1))
    draw <- ifelse(multi_draw %in% move_states, multi_draw, 0)
    return(draw)
    


}


#' Extract the infectious transition states
#'
#' @param sus_state the integer of the susceptible state that the susceptible agent is in
#' @param inf_states the integers of the infectious states 
#' @param sus_inf_arr a K x K x K array where entry ijk=1 means that an interaction between an agent in susceptible state i can put an agent in infectious state j into state k with non-zero probability.
#' @return vector of the possible states an infectious in inf_state can put susceptible in sus_state in
extract_transition_states <- function(sus_state, inf_states,
                                      sus_inf_arr){
    zero_prob_states <- integer()
    for(inf_state in inf_states){
        zero_prob_states <- c(zero_prob_states,
                              which(
                                  sus_inf_arr[sus_state,
                                              inf_state,] > 0))
    }
    if(length(zero_prob_states) == 0) stop("no valid transitions!")
    return(unique(zero_prob_states))

}
                                                 


#' Rescale agent probs to not include getting infected
#'
#' @param agent_prob vector of length K, all pos, sum to one
#' @param zero_prob_states vector with integers corresponding to which states the agent can no longer transfer to on account of it not being infected
#' @return updated agent_prob
rescale_agent_prob <- function(agent_prob,
                               zero_prob_states){
    agent_prob[zero_prob_states] <- 0
    if(isTRUE(all.equal(sum(agent_prob), 0))) stop("no valid transfers available")
    agent_prob <- agent_prob / sum(agent_prob)
    return(agent_prob)
}

#' Extract the total number of agents in each state for given time
#' 
#' @param a T+1 x N matrix where T+1 is the final time step and N is the number of agents where entry tn = k means that agent n at time t is in state k.
#' @param K number of states 1:K
#' @param tt current time step
#' @return X a table of current state totals
get_totals <- function(agent_data, tt, K){
    states <- factor(agent_data[tt, ], levels = 1:K)
    X <- table(states)
    return(X)

}



#' Extract the indices of agents in certain states
#'
#' @param states vector of integers subset of 1:K
#' @param agent_data T x N matrix where T+1 is the final time step and N is the number of agents where entry tn = k means that agent n at time t is in state k.
#' @param tt current time step
extract_indices <- function(states, agent_data, tt){
    current_states <- agent_data[tt, ]
    indices <- which(current_states %in% states)
    return(indices)
}


#' Estimate the AM from given parameters
#'
#' @param theta vector of parameters
#' @param K total number of states 1:K
#' @param inf_param_inds which indices correspond to probabilities of being infected by an individual infectious agent for p.  Default is 1 and it is beta
#' @param agent_data T x N matrix where T+1 is the final time step and N is the number of agents where entry tn = k means that agent n at time t is in state k.
#' @param nbr_list  of length N where entry n is a vector of indices of neighbors, that is people who share an environment assignment of non-zero for the same category.  The list entry has a 0 element if it has no neighbors
#' @return a T x K matrix where entry ti is the number of agents in state i at time t
estimate_AM <- function(theta, K,
                        inf_param_inds = 1,
                        agent_data,
                        nbr_list,
                        transmission_fxn = SI){
    init_X <- get_totals(agent_data, tt = 1, K)
    ## TODO
}


#' Estimate the SI AM from given parameters
#'
#' @param theta vector of parameters
#' @param agent_data T x N matrix where T+1 is the final time step and N is the number of agents where entry tn = k means that agent n at time t is in state k.
#' @param nbr_list  of length N where entry n is a vector of indices of neighbors, that is people who share an environment assignment of non-zero for the same category.  The list entry has a 0 element if it has no neighbors
#' @return a T x 2 matrix where entry ti is the number of agents in state i at time t
#' @details i.e. this is the expected value of the number of agents in each state given the previous agent states for the AM
estimate_AM_SI <- function(theta, agent_data,
                           nbr_list){
    N <- ncol(agent_data)
    T <- nrow(agent_data)
    init_S <- sum(agent_data[1, ] == 1)
    X_mat <- matrix(0, nrow = T, ncol = 2)
    X_mat[1, 1] <- init_S
    for (tt in 2:T){
        inf_inds <- which(agent_data[tt-1, ] == 2)
        sus_inds <- which(agent_data[tt-1, ] == 1)
        p <- sapply(sus_inds, function(nn){
            nbr_inds <- nbr_list[[nn]]
            inf_nbr_inds <- find_inf_nbrs(inf_inds, nbr_inds)
            if(all(inf_nbr_inds > 0)){
                n_nbr_inds <- length(inf_nbr_inds)
                p <- 1 - (1- theta[1] / N)^n_nbr_inds
            } else {
                0
            }
        })
        X_mat[tt, 1] <- X_mat[tt-1, 1] - sum(p)
    }
    
    X_mat[, 2] <- N - X_mat[ ,1]
    return(X_mat)
    

}
