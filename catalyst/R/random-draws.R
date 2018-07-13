
#' Draw a multinomial to update agents
#'
#' @param agent_probs NxK matrix where entry n k is probability of transitioning from its current state (assumed given) and state k
#' @param bounds numeric vector of size K + 1.  Default is NULL
#' @param N number of agents.  Default is number of rows in agent_probs
#' @return vector of new states
draw_multinom <- function(agent_probs, bounds = NULL,
                          N = nrow(agent_probs)){
    draws <- runif(n = N) # draw N uniforms between 0 and 1
    new_states <- rep(1, N)
    if(!is.null(bounds)){  ## slower AM step
        agent_cum <- t(apply(agent_probs, 1, cumsum)) # get cumulative probs for each agent (these are the boundaries)
    ## Add a zero column since we need K+1 boundaries to have K intervals
        agent_cum <- cbind(0, agent_cum)
        for(kk in 2:(ncol(agent_probs)+1)){
            new_states <- ifelse(draws < agent_cum[, kk] &
                                 draws >= agent_cum[, kk-1],
                                 kk-1,
                                 new_states)
        }
    } else {  ## Faster(?) CM step
        for(kk in 2:(ncol(agent_probs)+1)){
            new_states <- ifelse(draws < bounds[kk] &
                                 draws >= bounds[kk],
                                 kk-1,
                                 new_states)

        }

    }

    return(new_states)
    
                   
    


}
