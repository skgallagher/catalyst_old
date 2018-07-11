
#' Draw a multinomial to update agents
#'
#' @param agent_probs NxK matrix where entry n k is probability of transitioning from its current state (assumed given) and state k
#' @return vector of new states
draw_multinom <- function(agent_probs){
    draws <- runif(n=nrow(agent_probs)) # draw N uniforms between 0 and 1
    agent_cum <- apply(agent_probs, 1, cumsum) # get cumulative probs for each agent (these are the boundaries)
    new_states <- integer(nrow(agent_probs)
    for(kk in 1:ncol(agent_probs){
        new_states <- ifelse(draws > agent_probs[, kk],
                             new_states,
                             kk)
    }
    return(new_state)
    
                   
    


}
