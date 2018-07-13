
#' Draw a multinomial to update agents
#'
#' @param agent_probs NxK matrix where entry n k is probability of transitioning from its current state (assumed given) and state k
#' @param bounds numeric vector of size K + 1.  Default is NULL.  It is cumulative sum.  First entry is 0.
#' @param N number of agents.  Default is number of rows in agent_probs
#' @return vector of new states
draw_multinom <- function(agent_probs, bounds = NULL,
                          N = nrow(agent_probs)){
    draws <- runif(n = N) # draw N uniforms between 0 and 1
    if(is.null(bounds)){  ## slower AM step
        agent_cum <- t(apply(agent_probs, 1, cumsum)) # get cumulative probs for each agent (these are the boundaries)
    ## Add a zero column since we need K+1 boundaries to have K intervals
        agent_cum <- cbind(0, agent_cum)
        K <- ncol(agent_probs)
        new_states <- assign_interval(K, N, agent_cum, draws)
    } else {  ## Faster(?) CM step
        K <- length(bounds) - 1
        ## Some hacking to get matrix/vector indexing to align
        bounds_df <- t(as.data.frame(bounds, ncol = K + 1))
        new_states <- assign_interval(K, N, bounds_df, draws)
    }

    return(new_states)
}


#' Assign our random uniform to a multinomial draw
#'
#' @param K number of compartments
#' @param N number of agents
#' @param bounds data frame of dim 1x( K +1) where first entry is 0, monotonic and sums to 1 OR matrix of dim N x (K+1) where each entry in first column is 0, and each row is monotonic and sums to 1
#' @param draws vector of random uniform draws of size N
#' @return vector of new state assignments
assign_interval <- function(K, N, bounds, draws){
    new_states <- rep(1, N)
    for(kk in 2:(K+1)){ ## Could probably simplify to Reduce
        new_states <- ifelse(draws < bounds[, kk] &
                             draws >= bounds[, kk-1],
                             kk-1,
                             new_states)
    }
    return(new_states)

}

