## SKG
## Catalyst v2
## Try try again


#' Main function to run the CM/AM
#'
#' @param ll simulation number
#' @param trans_fxn(t, agent_data, theta) a function that returns the KxK transition matrix where K is the number of states.  Entry ij is the POSTIVE number of agents moving from state i to state j from time t-1 to t for t=1, \dots, T.
#' @param theta disease parameters.  Default is c(beta) for basic SI model
#' @param agent_data a T+1 x N matrix where T+1 is the final time step and N is the number of agents where entry tn = k means that agent n at time t is in state k.
#' @param nbr_list of length N where entry n is a vector of indices of neighbors, that is people who share an environment assignment of non-zero for the same category.  The list entry has a 0 element if it has no neighbors
#' @param states numeric vector indicating the different states an agent may take.  Default is 1:2
#' @param inf_states subset of states that indicate the infectious states.  Default is 2.
#' @param sus_states subset of states that indicate the susceptible states.  Default is 1
#' @param sus_inf_arr a K x K x K array where entry ijk=1 means that an interaction between an agent in susceptible state i can put an agent in infectious state j into state k with non-zero probability. 
#' @param do_AM logical.  Default is TRUE, which runs the AM interactions
#' @return summarized simulation of the CM/AM
catalyze <- function(ll, trans_fxn,
                     theta = c(beta),
                     agent_data,
                     nbr_list,
                     states = 1:2,
                     inf_states = 2,
                     sus_states = 1,
                     sus_inf_arr,
                     do_AM = TRUE){

    ## Extract parameter sizes
    T <- nrow(agent_data)
    N <- ncol(agent_data)
    K <- length(states)


    for(tt in 1:(T-1)){

        X <- get_totals(agent_data, tt, K) # total number of agents at this time step vec of size K

        agent_probs <- extract_prob_trans(trans_fxn, X, theta)
        if(do_AM){
            ## Interact the infectious with susceptibles
            inf_indices <- extract_indices(inf_states, agent_data, tt)
            sus_indices <- extract_indices(sus_states, agent_data, tt)
            ## Will change prob to 1 if infected, otherwise will rescale other probs where appropriate
            agent_probs <- interact_agents(inf_indices, sus_indices,
                                           agent_data, nbr_list, tt, 
                                           sus_inf_arr,
                                           agent_probs)
        }
        ## THE MULTINOMIAL UPDATE
        ## Could be made faster to actually do a full multinomial update probably like from a package
        agent_data[tt+1, ] <- draw_multinom(agent_probs)
    }

    ## Summarize the agents in a more applicable manner such as D or X
    sum_sim <- summarize_agent_data(agent_data)
    return(sum_sim)
    

}
                     
#' Get the totals of agents at the current time step
#'
#' @param agent_data  a T+1 x N matrix where T+1 is the final time step and N is the number of agents where entry tn = k means that agent n at time t is in state k.
#' @param tt current time step
#' @param K total number of states
#' @return vector of length K where each entry k is the number of agents in state k at time tt
get_totals <- function(agent_data, tt, K){
    X <- table(factor(agent_data[tt, ], levels = 1:K))
    return(as.integer(X))
}
