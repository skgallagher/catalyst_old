
#' Draw a multinomial to update agents
#'
#' @param agent_probs NxK matrix where entry n k is probability of transitioning from its current state (assumed given) and state k
#' @return vector of new states
draw_multinom <- function(agent_probs){
    draws <- runif(n=nrow(agent_probs)) # draw N uniforms between 0 and 1
    agent_cum <- t(apply(agent_probs, 1, cumsum)) # get cumulative probs for each agent (these are the boundaries)
    ## Add a zero column since we need K+1 boundaries to have K intervals
    agent_cum <- cbind(0, agent_cum)
    new_states <- rep(1, nrow(agent_probs))
    for(kk in 2:(ncol(agent_probs)+1)){

        new_states <- ifelse(draws < agent_cum[, kk] &
                             draws >= agent_cum[, kk-1],
                             kk-1,
                             new_states)
    }
    return(new_states)
    
                   
    


}
