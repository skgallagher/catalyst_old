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
#' @param env_list list with
#' "init_env_table" a contingency table of number of individuals in each each cross section.  Each dimension name of the table corresponds to a number 1 to E_j, the number of total specific elements in Environment j
#' "E" total number of environment types (e.g. school and workplace is 2)
#' "N" total number of agents 
run_AM_step <- function(tt, N, K,
                        current_agent_status,
                        current_base_probs,
                        neighbor_list,
                        disease_params_list,
                        agent_list,
                        env_list){

    ## First initialize agent_probs based on current_probs
    ## Then loop over INFECTED agents
    ## Look at their SUSCEPTIBLE NEIGHBORS
    ## Have INFECTED (possibly) infect SUSCEPTIBLE NEIGHBOR
    ## Modify agent_probs IF infected
    ## Remove SUSCEPTIBLE FROM INFECTION LIST?

    agent_probs <- make_n_states_df(current_base_probs,
                                    current_agent_status)

    current_states <- get_agent_state_inds(current_agent_status,
                                           disease_params_list$infection_states,
                                           disease_params_list$susceptible_states)

    rolling_sus_inds <- current_states$susceptible_inds
    if(current_states$infectious_inds > 0){
        for (inf_ind in current_states$infectious_inds){
            neighbor_inds <- neighbor_list[[inf_ind]] # Neighbors
            ## Find susceptible neighbors
            ## TODO:  Could probably add to infection process
            susceptible_neighbors <- intersect(rolling_sus_inds,
                                               neighbor_inds)
            if(length(susceptible_neighbors) > 0){
                agent_probs[susceptible_neighbors, ] <- infect_neighbors(agent_probs[susceptible_neighbors,b],
                                                                         disease_params_list,
                                                                         agent_list$agent_vars)
                rolling_sus_inds <- update_susceptible_inds(rolling_sus_inds, agent_probs[susceptible_neighbors, ],
                                                            susceptible_neighbors)

            }
        }
        
    }                                                           
    return(agent_probs)
    


}

#' Get current infection/susceptible indices
#'
#' @param current_agent_status integer vector of length N consisting of current status of agent
#' @param infection_states vector of indices of infection states, e.g. in SIR I is 2
#' @param susceptible_states vector of indices of susceptible states, e.g. in SIR S is 1
#' @return list with
#' "infectious_inds" indices of infected individuals
#' "susceptible_inds" indices of susceptible individuals
get_agent_state_inds <- function(current_agent_status,
                                 infection_states,
                                 susceptible_states){
    inf_states <- which(current_agent_status %in% infection_states)
    sus_states <- which(current_agent_status %in% susceptible_states)
    return(list(infectious_inds = inf_states,
                susceptible_inds = sus_states))


}

#' Update the rolling susceptible individuals
#'
#' @param rolling_sus_inds previous susceptible individual indices
#' @param sub_agent_probs submatrix of agent_probs.  sub_agent_probs is a length(rolling_sus_neighbors) x K matrix where entry ij corresponds to prob of rolling_sus_neighbors[i] chance of moving to state j based on current state
#' @param infection_states vector of infection indices
#' @param susceptible_neighbors indices of susceptible neighbors from the ORIGINAL agent_probs
#' @return a new vector of rolling sus_neighbors
#' @details Shrink the susceptible list.  If infectious probs sum to 1, then this agent has been infected so we should take it off the susceptible list
update_susceptible_inds <- function(rolling_sus_inds,
                                    sub_agent_probs,
                                    infection_states,
                                    susceptible_neighbors){
    inf_cum_prob <- rowSums(sub_agent_probs[, infection_states])
    new_infections_inds <- susceptible_neighbors[inf_cum_prob >= (1 - 1e-10)]
    if(length(new_infections) > 0){
        new_susceptible_inds <- setdiff(rolling_sus_inds,
                                        new_infections_inds)
    } else {
        new_susceptible_inds <- rolling_sus_inds
    }
    
    return(new_susceptible_inds)
    }
