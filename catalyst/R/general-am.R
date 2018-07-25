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
#' CM_fxn - the compartment model function
#' "transmission_probs" n_infectious_states x n_infection_states where entry ij is prob of agent in infectious state i agent to state j.  NOTE: n_infectious_states are those that CAN transmit the disease.  n_infection_states can include latently infectious individuals
#' "contact_probs" probability of contact with another agent
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
#' @return updated agent_probs
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

    agent_probs <- base_to_agent_probs(current_base_probs,
                                    current_agent_status)

    current_states <- get_agent_state_inds(current_agent_status,
                                           disease_params_list$infection_states,
                                           disease_params_list$susceptible_states)

    rolling_sus_inds <- current_states$susceptible_inds
    if(length(current_states$infectious_inds) > 0){
        for (inf_ind in current_states$infectious_inds){
            browser()
            neighbor_inds <- neighbor_list[[inf_ind]] # Neighbors
            ## Find susceptible neighbors
            ## TODO:  Could probably add to infection process
            susceptible_neighbors <- intersect(rolling_sus_inds,
                                               neighbor_inds)
            if(length(susceptible_neighbors) > 0){
                agent_probs[susceptible_neighbors, ] <- infect_neighbors(
                    current_agent_status[inf_ind],
                    agent_probs[susceptible_neighbors, ],
                    susceptible_neighbors,
                    disease_params_list,
                    agent_list$agent_vars
                )
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
    inf_cum_prob <- rowSums(sub_agent_probs[, infection_states, drop = FALSE])
    new_infections_inds <- susceptible_neighbors[which(inf_cum_prob >= (1 - 1e-10))]
    if(length(new_infections_inds) > 0){
        new_susceptible_inds <- setdiff(rolling_sus_inds,
                                        new_infections_inds)
    } else {
        new_susceptible_inds <- rolling_sus_inds
    }
    
    return(new_susceptible_inds)
}



#' Infect and transmit disease
#'
#' @param infector_state state the infector is currently in
#' @param infection_states vector of infection state indices
#' @param sub_agent_probs submatrix of agent_probs.  sub_agent_probs is a length(rolling_sus_neighbors) x K matrix where entry ij corresponds to prob of rolling_sus_neighbors[i] chance of moving to state j based on current state
#' @param infection_states vector of infection indices
#' @param susceptible_neighbors indices of susceptible neighbors from the ORIGINAL agent_probs
#' @param disease_params_list list including
#' "K" the number of states
#' "infection_states" the indices of the states that can infect others
#' "susceptible_states" the indices of the states that are susceptible
#' "init_vals" vector of size K that sums to N, initial values in each of the states at time t=0
#' "params" vector of disease parameters (e.g. beta, gamma)
#' "params_names" optional vector of parameter names
#' T - total number of time steps, 0 to T-1 inclusive
#' CM_fxn - the compartment model function
#' "transmission_probs" n_infectious_states x K where entry ij is prob of agent in infectious state i agent to state j.  NOTE: n_infectious_states are those that CAN transmit the disease.  
#' "contact_probs" probability of contact with another agent
#' @param agent_vars covariates of the agent
#' @return updated matrix of sub draws
infect_neighbors_draws <- function(infector_state, 
                                   sub_agent_probs,
                                   susceptible_neighbors,
                                   disease_params_list,
                                   agent_vars){

    contact_probs <- disease_params_list$contact_probs
    transmission_probs<- disease_params_list$transmission_probs
    ## First see if there is a contact
    successful_contacts <- rbinom(length(susceptible_neighbors), 1,
                                  contact_probs)
    contacted_inds <- which(successful_contacts > 0)
    ## Next transmit the disease
    agent_probs <- transmission_probs[rep(infector_state, length(contacted_inds)), ]
    new_states <- draw_multinom(agent_probs, bounds = NULL,
                                length(contacted_inds))
    ## Update the agent probailities, putting a 1 in the new state
    updated_sub_agent_probs <- matrix(0, nrow = nrow(sub_agent_probs),
                                      ncol = ncol(sub_agent_probs))
    for(jj in 1:length(new_states)){
        agent_index <- contacted_inds[jj]
        updated_sub_agent_probs[agent_index,
                                new_states[jj]] <- 1
    }
                                      
                                  

    return(updated_sub_agent_probs)
}
