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
#' "transmission_probs" n_infectious_states x n_infection_states where entry ij is prob of agent in infectious state i agent to state j.  NOTE: n_infectious_states are those that CAN transmit the disease.  n_infection_states can include latently infectious individuals
#' "contact_probs" probability of contact with another agent
#' "do_plugin_probs" logical.  Should we dynamically update probabilities based on previous results of simulation?  See details.
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




    current_states <- get_agent_state_inds(current_agent_status,
                                           disease_params_list$infection_states,
                                           disease_params_list$susceptible_states)

    sus_inds <- current_states$susceptible_inds
    inf_inds <- current_states$infectious_inds
    n_sus <- length(sus_inds)
    n_inf <- length(inf_inds)


    if(is.null(disease_params_list$do_plugin_probs)){ ## So all my tests don't break
        disease_params_list$do_plugin_probs <- FALSE
    }
    if(disease_params_list$do_plugin_probs){ # Should reduce variability of AM.  See document for more details
        disease_params_list$transmission[tt+1,, ] <- update_transmission_probs_SIR(
            disease_params_list$transmission[tt+1,, ],
            current_states,
            disease_params_list)
    }
    
    agent_probs <- base_to_agent_probs(current_base_probs,
                                           current_agent_status)
    
    
    if(n_sus > 0 & n_inf > 0){ # If there are both susceptibles and infectious left
        ## Loop over susceptsibles
        for(sus_ind in sus_inds){
            ## Find infectious neighbors
            inf_nbr_inds <- intersect(inf_inds, neighbor_list[[sus_ind]])
            current_sus_state <- current_agent_status[sus_ind]
            if(length(inf_nbr_inds) > 0){
                agent_probs[sus_ind, ] <- infect_susceptible_from_nbrs(tt,
                                                                       current_agent_status,
                                                                       inf_nbr_inds,
                                                                       disease_params_list,
                                                                       current_sus_state)
            } else {
                ## Stay where you are/ move to another non-infection state
                agent_probs[sus_ind, infection_states] <- 0
                if(sum(agent_probs[sus_ind,]) == 0) stop("0 probability of transfer")
                agent_probs[sus_ind, ] <- agent_probs[sus_ind, ] / sum(agent_probs[sus_ind, ])
            }


        }
       
        
    } else if (n_sus == 0){ 
        ## If no susceptible do nothing

    } else { # If some susceptible, and no infectious
        infection_states <- disease_params_list$infection_states
        ## Set probability of infection to 0, rescale other state changes
        agent_probs[, infection_states] <- 0
        if(any(rowSums(agent_probs) == 0)) stop("0 probability of transfer")
        agent_probs[sus_inds, ] <- agent_probs[sus_inds, ]  /
            sum(agent_probs[sus_inds, ] )

    }
    return(agent_probs)
    


}

#' Infect a susceptible from infectious neigbhors
#'
#' @param tt current step
#' @param current_agent_status integer vector of length N consisting of current status of agent
#' @param inf_nbr_inds indices of infectious neighbors
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
#' @param current_sus_state current susceptible state of individual
#' @return vector of size K with a 1 in the new state of the susceptible agent and 0 elsewhere.  If the susceptible is not infected, he will stay in current state
infect_susceptible_from_nbrs <- function(tt,
                                         current_agent_status,
                                         inf_nbr_inds,
                                         disease_params_list,
                                         current_sus_state){
    ## TODO: make more general
    ## Add in contact probabilities
    successful_contacts <- rbinom(length(inf_nbr_inds), 1,
                                  disease_params_list$contact_probs)
    ## Make agent probabilities
    transmission_probs <- disease_params_list$transmission_probs[tt+1, ,]
    infector_states <- current_agent_status[inf_nbr_inds[successful_contacts]]
    agent_probs <- transmission_probs[infector_states, , drop = FALSE]
    ## This gets infections individually
    new_states <- draw_multinom(agent_probs, bounds = NULL,
                                N = length(infector_states))
    new_state <- determine_sus_infection(new_states, disease_params_list$infection_states,
                                         current_sus_state)
    agent_prob <- numeric(disease_params_list$K)
    agent_prob[new_state] <- 1
    return(agent_prob)
 


}

#' Determine if at least one infection occurred from infectious
#'
#' @param new_states vector of length of current successfully contacted infectious neighbors where the entry is the result of the infection of the neighbor onto the susceptible.  (e.g. for the SIR model, if the susceptible is infected, at least 1 entry will be 2 (for I).
#' @param infection_states vector of possible infection states (e.g. for SIR I=2)
#' @return a single integer indicating which state the susceptible agent is going to.
determine_sus_infection <- function(new_states, infection_states,
                                    current_sus_state){
    infections <- new_states[new_states %in% infection_states]
    if(length(infections) > 0){
        ## Return the most common infection type
        mode_infection <- as.numeric(names(sort(-table(infections)))[1])
        return(mode_infection)
    } else {
        return(current_sus_state)
    }


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
