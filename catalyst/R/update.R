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
    tt <- id(tt)
    return(base_probs)
}


#' Update the environments
#'
#' @param env_status a NxE matrix where entry ij is agent i's value assignment to environment j.  An assignment of 0 corresponds to a NULL assignment.  Agents with value 0 do not belong to the same environment
#' @param tt current time step
update_env <- function(env_status, tt){
    tt <- id(tt)
    return(env_status)
}
