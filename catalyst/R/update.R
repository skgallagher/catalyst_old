## SKG
## July 13, 2018
## Update functions

#' Update the agent states from current step to next
#'
#' @param  agent_probs N x K matrix where entry ni is probability of moving to state i conditioned on current state for agent n
#' @param current_agent_vec integer vector of length N consisting of current status of agent
#' @param current_base_probs KxK matri where entry ij is prob of transfer from i to j conditioned on current time step
#' @param do_AM logical.  Default is FALSE
update_agents <- function(agent_probs,
                          current_agent_vec,
                          current_base_probs,
                          do_AM = FALSE){
    if(do_AM){
        new_states <- draw_multinom(agent_probs, bounds = NULL,
                                    N = nrow(agent_probs))
    } else {
        N <- length(current_agent_vec)
        K <- nrow(current_base_probs)
        ## Sort agents in terms of current status (at least store index)
        new_indexing_list <- lapply(1:K, function(kk){
            inds <- which(current_agent_vec == kk)
            if(length(inds) > 0)   return(inds)
            return(NULL)
            
        })
        new_indexing_vec <- unlist(new_indexing_list)
        ## Get bounds for groups
        CM_bounds_list <- lapply(1:K, get_CM_bounds_list,
                                 current_base_probs,
                                 new_indexing_list)
        ## Draw multinomials
        draws <- unlist(sapply(1:length(CM_bounds_list),
                        function(ii){
                            N <- CM_bounds_list[[ii]]$N
                            if(N > 0){
                                out <- draw_multinom(agent_probs = NULL,
                                              bounds = CM_bounds_list[[ii]]$bounds,
                                              N = N)
                                return(out)
                            }
                            
                            
                        }))
        names(draws) <- NULL
                        
        ## Reorder according to indexing list
        new_states <- draws[new_indexing_vec]
        
    }
    return(new_states)

}

#' Get the homogeneous bounds for a given group
#' 
#' @param kk current compartment
#' @param current_base_probs  KxK matrix where entry ij is cum prob of transfer from i to j conditioned on current time step
#' @param N number of people in the category
#' @param indexing_list list of indices of agents currently in the same group
#' @return list of N and bounds
get_CM_bounds_list <-function(kk, current_base_probs,
                              indexing_list){
    N <- length(indexing_list[[kk]])
    bounds <- c(0, cumsum(current_base_probs[kk, ]))
    return(list(N=N, bounds= bounds))
}


#' Update the base probabilities
#'
#' @param base_probs T x K x K array where entry tij to the probability of transferring from state i to j from time t-1 to t
#' @param tt current time step
#' @return updated base_probs
update_base_probs <- function(base_probs, tt){
    tt <- identity(tt)
    return(base_probs)
}


#' Update the environments
#'
#' @param env_status a NxE matrix where entry ij is agent i's value assignment to environment j.  An assignment of 0 corresponds to a NULL assignment.  Agents with value 0 do not belong to the same environment
#' @param tt current time step
update_env <- function(env_status, tt){
    tt <- identity(tt)
    return(env_status)
}


#' Update base probs according to plug-in
#'
#' @param current_transmission_probs KxK matrix where entry i j is the probability of an agent  being infected by an agent in state i to state j
#' @param current_states list with
#' "infectious_inds" indices of infected individuals
#' "susceptible_inds" indices of susceptible individuals
#' @param disease_params_list list including
#' "K" the number of states
#' "infection_states" the indices of the states that can infect others
#' "init_vals" vector of size K that sums to N, initial values in each of the states at time t=0
#' "params" vector of disease parameters (e.g. beta, gamma)
#' "params_names" optional vector of parameter names
#' T - total number of time steps, 0 to T-1 inclusive
#' CM_fxn - the compartment model function
#' "transmission_probs" n_infectious_states x n_infection_states where entry ij is prob of agent in infectious state i agent to state j.  NOTE: n_infectious_states are those that CAN transmit the disease.  n_infection_states can include latently infectious individuals
#' "contact_probs" probability of contact with another agent
#' "do_plugin_probs" logical.  Should we dynamically update probabilities based on previous results of simulation?  See details.
#' @return updated current_transmission_probs
#' @details specifically for SIR
update_transmission_probs_SIR <- function(current_transmission_probs,
                                          current_states,
                                          disease_params_list){
    N <- sum(disease_params_list$init_vals)
    I <- length(current_states$infectious_inds)  # THIS is hat(I), observed value not theoretical
    p <- I * disease_params_list$params[1] / N # marginal probability of infection for a given sus
    ## p <- current_transmission_probs[2, 2]  # non-plug-in values
    prob_of_transmission <- 1 - (1 - p) ^ ( 1 / I )

    current_transmission_probs[2,2] <- prob_of_transmission
    current_transmission_probs[2,1] <-  1 - prob_of_transmission
    
    return(current_transmission_probs)
}

#' Update base probs based on plug-in estimator for I
#'
#' @param current_base_probs KxK matrix where entry ij is prob of transfer from i to j conditioned on current time step
#' @param disease_params_list list including
#' "K" the number of states
#' "infection_states" the indices of the states that can infect others
#' "init_vals" vector of size K that sums to N, initial values in each of the states at time t=0
#' "params" vector of disease parameters (e.g. beta, gamma)
#' "params_names" optional vector of parameter names
#' T - total number of time steps, 0 to T-1 inclusive
#' CM_fxn - the compartment model function
#' "transmission_probs" n_infectious_states x n_infection_states where entry ij is prob of agent in infectious state i agent to state j.  NOTE: n_infectious_states are those that CAN transmit the disease.  n_infection_states can include latently infectious individuals
#' "contact_probs" probability of contact with another agent
#' "do_plugin_probs" logical.  Should we dynamically update probabilities based on previous results of simulation?  See details.
#' @param current_agent_status integer vector of length N consisting of current status of agent
#' @return updated current_base probs based on observed number of I rather than expected
update_base_probs_SIR <- function(current_base_probs,
                                  disease_params_list,
                                  current_agent_status){
    
    current_states <- get_agent_state_inds(
        current_agent_status,
        disease_params_list$infection_states,
        disease_params_list$susceptible_states
    )

    
    N <- sum(disease_params_list$init_vals)
    I <- length(current_states$infectious_inds) 
    p <- I * disease_params_list$params[1] / N
    current_base_probs[1,2] <- p
    current_base_probs[1,1] <- 1 - p
    return(current_base_probs)
    

}
