## SKG
## July 23, 2018
## Running out of time lol
## The agent-interaction part!

#' Run the pairwise interaction infection steps
#'
#' @param tt current time step 0 to T-1
#' @param N total number of agents
#' @param K total number of states
#' @param current_agent_status integer vector of length N consisting of current status of agent
#' @param current_base_probs KxK matri where entry ij is prob of transfer from i to j conditioned on current time step
#' @param env_status a NxE matrix where entry ij is agent i's value assignment to environment j.  An assignment of 0 corresponds to a NULL assignment.  Agents with value 0 do not belong to the same environment
#' @param neighbor_list list of length N where entry n is a vector of indices of neighbors, that is people who share an environment assignment of non-zero for the same category.  The list entry has a 0 element if it has no neighbors
#' @param disease_params_list list including
#' "K" the number of states
#' "infection_states" the indices of the states that can infect others
#' "init_vals" vector of size K that sums to N, initial values in each of the states at time t=0
#' "params" vector of disease parameters (e.g. beta, gamma)
#' "params_names" optional vector of parameter names
#' T - total number of time steps, 0 to T-1 inclusive
#' CM_fxn - the compartment model function
#' @param agent_list list with
#' "init_CM_vals" vector of size K where entry k is the size of state k at time 0. 
#' "N" number of total agents
#' "K" number of total disease states
#' "T" max time, integer value
#' "agent_vars" covariates of agents 
#' @return Modified agent_probs.  NxK matrix where entry ni is agent n's current probability of transitioning to state k
run_AM_step <- function(tt, N, K,
                        current_agent_status,
                        current_base_probs,
                        env_status,
                        neighbor_list,
                        disease_params_list,
                        agent_list){

    ## First initialize agent_probs based on current_probs
    ## Then loop over INFECTED agents
    ## Look at their SUSCEPTIBLE NEIGHBORS
    ## Have INFECTED (possibly) infect SUSCEPTIBLE NEIGHBOR
    ## Modify agent_probs IF infected
    ## Remove SUSCEPTIBLE FROM INFECTION LIST?
    


}
