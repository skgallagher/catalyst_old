#' Run catalyst assuming homogeneous network, different p
#'
#' @param ll simulation number
#' @param p vector of size F, ps for the different groups
#' @param agent_data a T+1 x N matrix where T+1 is the final time step and N is the number of agents where entry tn = k means that agent n at time t is in state k.
#' @param grouping_vec argument passed to p_fxn
#' @param p_fxn function to take p to vector of pn
#' @return summarized simulation
catalyst_am_p_si <- function(ll, p, agent_data,
                             grouping_vec,
                             p_fxn = p_id){
    T <- nrow(agent_data)
    for(tt in 1:(T-1)){
        pn <- p_fxn(p, grouping_vec)
        sus_inds <- which(agent_data[tt,] == 1)
        agent_data[tt+1,] <- 2
        agent_data[tt+1, sus_inds] <- rbinom(n = length(sus_inds),
                        size = 1,
                        prob = pn) + 1
        
        
    }
    sum_sim <- summarize_agent_data(agent_data, K = 2)
    sum_sim$ll <- ll
    return(sum_sim)



}


#' Run catalyst assuming homogeneous network, different p
#'
#' @param L total number of simulations
#' @param p vector of size F, ps for the different groups
#' @param agent_data a T+1 x N matrix where T+1 is the final time step and N is the number of agents where entry tn = k means that agent n at time t is in state k.
#' @param grouping_vec argument passed to p_fxn
#' @param p_fxn function to take p to vector of pn
#' @return list of summarized simulations
sim_am_p_si <- function(L, p, agent_data,
                        grouping_vec,
                        p_fxn){

    out_list <- lapply(1:L, catalyst_am_p_si,
                       p = p, agent_data = agent_data,
                       grouping_vec = grouping_vec,
                       p_fxn = p_fxn)
    return(out_list)

}

    
