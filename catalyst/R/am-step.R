## SKG
## July 18, 2018
## AM Step

#' Run the individual interaction part of model
#'
#' @return updated agent_probs N x K matrix where entry ni is probability of moving to state i conditioned on current state for agent n
#' @param tt current time step - 0 to T-2
#' @param N total number of agents
#' @param K number of compartments
#' @param current_agent_status integer vector of length N consisting of current status of agent
#' @param agent_probs N x K matrix where entry ni is probability of moving to state i conditioned on current state for agent n
#' @param current_base_probs KxK matri where entry ij is prob of transfer from i to j conditioned on current time step
#' @param env_status a NxE matrix where entry ij is agent i's value assignment to environment j.  An assignment of 0 corresponds to a NULL assignment.  Agents with value 0 do not belong to the same environment
#' @param neighbor_list list of length N where entry n is a vector of indices of neighbors, that is people who share an environment assignment of non-zero for the same category.  The list entry has a 0 element if it has no neighbors
#' @param agent_vars covariates of agents.  Default is NULL 
#' @param disease_params_list - list of disease parameters.  One must be a named entry "infectious_states" which is an integer vector of states which can infect one another, eg for SIR it is 2 as I is the only one which an infect others
run_AM_step <- function(tt, N, K,
                        current_agent_status,
                        agent_probs,
                        current_base_probs,
                        env_status,
                        neighbor_list,
                        disease_params_list,
                        agent_vars= NULL){
    ## Step through population and have them interact to update/transmit agent_probs
    return(agent_probs)
}
