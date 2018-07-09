## Initialize agents, environment assignments, list of neighbors, 


#' Initialize the agents according to parameters in agent_list
#'
#' @param init_CM_vals vector of size K where entry k is the size of state k at time 0. 
#' @param N number of total agents
#' @param K number of total disease states
#' @param T max time, integer value
#' @return  N x (T+1) matrix of agent status with the first column (t=0) filled in with statuses corresponding to the init CM vals.  The other states are zero
initialize_agents <- function(init_CM_vals, N, K, T){
    agent_status <- matrix(0, ncol = (T + 1), nrow = N)
    if(sum(init_CM_vals) != N) stop("The initial CM values do not sum to N")
    if(length(init_CM_vals) != K) stop("There must be K initial values")
    ## Fill in initial values
    agent_status[, 1] <- rep(1:K, times  = init_CM_vals)
    return(agent_status)
}



#' Initialize environments
#'
#' @param env_list list with init_env_table a contingency table of number of individuals in each each cross section.  Each dimension name of the table corresponds to a number 1 to E_j, the number of total specific elements in Environment j.
#' @param N total number of agents
#' @param E total number of types of environments (e.g. schools and workplace -> E=2)
#' @return a NxE matrix where entry ij is agent i's value assignment to environment j.  An assignment of 0 corresponds to a NULL assignment.  Agents with value 0 do not belong to the same environment
initialize_env <- function(env_list, N, E){
    ## Convert table into data frame
    df <- as.data.frame(env_list$init_env_table,
                        stringsAsFactors = FALSE)
    ## Turn data frame into singular observations
    env_df <- df[rep((1:nrow(df))[df$Freq > 0],
                         times = df$Freq[df$Freq > 0]), 1:(ncol(df)-1), drop = FALSE]
    env_status <- as.matrix(env_df, drop = FALSE)
    env_status <- apply(env_status, 2, as.numeric)
    return(env_status)
}


#' Initialize the pre-computed neighbor list, which acts as a dictionary
#'
#' @param env_status a NxE matrix where entry ij is agent i's value assignment to environment j.  An assignment of 0 corresponds to a NULL assignment.  Agents with value 0 do not belong to the same environment
#' @param N total number of agents
#' @param E total number of environments
#' @return list of length N where entry n is a vector of indices of neighbors, that is people who share an environment assignment of non-zero for the same category
initialize_neighbors <- function(env_status, N, E){
    ## Initialize list
    neighbor_list <- vector(mode = "list", length = N)
    ## Loop over agents
    for(ii in 1:N){
        ## Loop over possible neighbors
        neighbor_inds<- NULL
        for(jj in 1:N){
            ## Loop over environments
            for(ee in 1:E){
                current_env <- env_status[ii, ee]
                ## NULL assignments (env =0) are NOT neighbors
                ## NEIGHBORS should not include self
                if( current_env > 0 & ii!=jj &
                    env_status[jj][ee] == current_env){
                    neighbor_inds <- c(neighbor_inds, jj)
                    break
                }
            }
            neighbor_list[[ii]] <- neighbor_inds
        }
    }
    return(neighbor_list)
}
