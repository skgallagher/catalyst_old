
#' Main function to run the interactive AM SI
#'
#' @param ll simulation number
#' @param trans_fxn NULL
#' @param theta probability of being infected.  Vector of length N where theta_n is the probability of being infected by an infectious agent for agent n given the agent is susceptible
#' @param agent_data a T+1 x N matrix where T+1 is the final time step and N is the number of agents where entry tn = k means that agent n at time t is in state k.
#' @param nbr_list of length N where entry n is a vector of indices of neighbors, that is people who share an environment assignment of non-zero for the same category.  The list entry has a 0 element if it has no neighbors
#' @param states numeric vector indicating the different states an agent may take.  Default is 1:2
#' @param inf_states subset of states that indicate the infectious states.  Default is 2.
#' @param sus_states subset of states that indicate the susceptible states.  Default is 1
#' @param sus_inf_arr NULL
#' @param do_AM TRUE
#' @param do_keep_agent_data logical indicating whether we should keep entire agent data. Default is FALSE
#' @param do_write_agent_data logical indicating whether we should write out agent_data. Default is FALSE
#' @param writing_dir Default is ".".  Where we write out results to
#' @return summarized simulation of the CM/AM
catalyze_am_si_homog <- function(ll,
                                 trans_fxn = NULL,
                                 theta,
                                 agent_data,
                                 nbr_list,
                                 states = 1:2,
                                 inf_states = 2,
                                 sus_states = 1,
                                 sus_inf_arr = NULL,
                                 do_AM = TRUE,
                                 do_keep_agent_data = FALSE,
                                 do_write_agent_data = FALSE,
                                 writing_dir = "."){

    T <- nrow(agent_data)
    N <- ncol(agent_data)
    K <- length(states)
   
    for(tt in 2:(T)){
        agent_data[tt, ] <- interactive_am_si_homog(theta,
                                                    agent_data[tt-1, ],
                                                    nbr_list,
                                                    states = 1:2,
                                                    inf_states = 2,
                                                    sus_states = 1
                                                    )
    }


    ## Summarize the agents in a more applicable manner such as D or X
    sum_sim <- summarize_agent_data(agent_data, K)
    sum_sim$ll <- ll
    if(do_write_agent_data){
        write_agent_data(writing_dir, ll, sum_sim$agent_data)
    }
    if(!do_keep_agent_data) sum_sim$agent_data <- NULL    
    return(sum_sim)
    



}

    



#' Interactive step for AM SI with homogeneous transition probs for each susceptible
#'
#' @param thetas probability of being infected.  Vector of length N where theta_n is the probability of being infected by an infectious agent for agent n given the agent is susceptible
#' @param current_agent_data a vector of agent N giving the states of the agents at the current time step
#' @param nbr_list of length N where entry n is a vector of indices of neighbors, that is people who share an environment assignment of non-zero for the same category.  The list entry has a 0 element if it has no neighbors
#' @param states numeric vector indicating the different states an agent may take.  Default is 1:2
#' @param inf_states subset of states that indicate the infectious states.  Default is 2.
#' @param sus_states subset of states that indicate the susceptible states.  Default is 1
#' @return next_agent_data a vector of agent N giving the states of the agents at the next time step
interactive_am_si_homog <- function(thetas,
                                   current_agent_data,
                                   nbr_list,
                                   states = 1:2,
                                   inf_states = 2,
                                   sus_states = 1
                                   ){


    N <- length(current_agent_data)
    thetas <- thetas * (current_agent_data %in% sus_states)
    inf_inds <- which(current_agent_data %in% inf_states)
    inf_nbrs <- get_inf_nbrs(nbr_list, inf_inds)
    n_inf_nbrs <- sapply(inf_nbrs, length)
    p <- 1 - (1 - thetas / N)^n_inf_nbrs
    next_agent_data <- rbinom(n = N, size = 1, prob = p) + current_agent_data
    return(next_agent_data)

    


}


#' Get the indices of the INFECTIOUS neighbors
#'
#' @param nbr_list of length N where entry n is a vector of indices of neighbors, that is people who share an environment assignment of non-zero for the same category.  The list entry has a 0 element if it has no neighbors.  Nbr list should NOT contain the agent itself.
#' @param inf_inds a vector of the currently infectious individual indices
#' @param inf_state subset of states that indicate the infectious states.  Default is 2.
#' @return list of length N where each entry is a vector of the currently infectious neighbor indices.  If there are no infectious neighbors, the entry is NULL
get_inf_nbrs <- function(nbr_list, inf_inds){
    inf_nbr_list <- lapply(nbr_list, function(vec){
        inds <- intersect(vec, inf_inds)
        if(length(inds) == 0) inds <- NULL
        return(inds)

    })
    return(inf_nbr_list)

}
