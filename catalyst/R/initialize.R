## Initialize agents, environment assignments, list of neighbors, 


#' Initialize the agents according to parameters in agent_list
#'
#' @param init_CM_vals vector of size K where entry k is the size of state k at time 0. 
#' @param N number of total agents
#' @param K number of total disease states
#' @param T max time, integer value
#' @return  (T+1) x N matrix of agent status with the first row (t=0) filled in with statuses corresponding to the init CM vals.  The other states are zero
initialize_agents <- function(init_CM_vals, N, K, T){
    
    agent_status <- matrix(0, nrow = (T + 1), ncol = N)
    if(sum(init_CM_vals) != N) stop("The initial CM values do not sum to N")
    if(length(init_CM_vals) != K) stop("There must be K initial values")
    ## Fill in initial values
    agent_status[1, ] <- rep(1:K, times  = init_CM_vals)
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
    env_status <- apply(env_status, 2, function(x) as.numeric(x))
    return(env_status)
}


#' Initialize the pre-computed neighbor list, which acts as a dictionary
#'
#' @param env_status a NxE matrix where entry ij is agent i's value assignment to environment j.  An assignment of 0 corresponds to a NULL assignment.  Agents with value 0 do not belong to the same environment
#' @param N total number of agents
#' @param E total number of environments
#' @return list of length N where entry n is a vector of indices of neighbors, that is people who share an environment assignment of non-zero for the same category.  The list entry has a 0 element if it has no neighbors
initialize_neighbors <- function(env_status, N, E){
    ## Initialize list
    neighbor_list <- vector(mode = "list", length = N)
    ## Loop over agents
    for(ii in 1:(N-1)){
        ## Loop over possible neighbors
        neighbor_inds <- NULL
        for(jj in (ii+1):N){
            ## Loop over environments

            for(ee in 1:E){
                current_env <- env_status[ii, ee]
                ## NULL assignments (env =0) are NOT neighbors
                ## NEIGHBORS should not include self
                if( current_env > 0  &
                    env_status[jj, ee] == current_env){
                    neighbor_list[[ii]] <- c(neighbor_list[[ii]], jj)
                    neighbor_list[[jj]] <- c(neighbor_list[[jj]], ii)
                    break
                }
            }
        }
        if(is.null(neighbor_list[[ii]])){
            neighbor_list[[ii]] <- 0
        }
    }
    if(is.null(neighbor_list[[N]])){
        neighbor_list[[N]] <- 0 ## Must manually check last entry
    }
  
    return(neighbor_list)
}


#' Find the initial probabilities of transition assuming homogeneity
#'
#' @param disease_list  List with
#' "params" - a numeric list of disease parameters (e.g. .1, .03)
#' "params_names" - optional variable names of params (e.g. beta, gamma)
#' "T" - max time.  The steps we take are 0, 1, ..., T inclusive
#' "init_vals" - vector of initial values of the states.  Must be nonnegative.  Length of vector is K, the number of states, and sum of values is N the constant number of individuals
#' @param CM_fxn output of make_CM_fxn
#' @return T x K x K matrix where entry t, i, j is the probability of an agent in compartment i transitioning to state j from time t-1 to t (because we start at 0)
#' @details TODO: currently hardcoded for SIR. Need to make general
initialize_probs <- function(disease_list, CM_fxn = SIR_diff){
    T <- disease_list$T
    K <- length(disease_list$init)
    N <- sum(disease_list$init)
    params <- disease_list$params
    

    ## Initialize array of dim TxKxK
    probs_mat <- array(0, c(T, K, K))

    ## Get ode_results
    ode_results <- sum_CM(disease_list, CM_fxn = CM_fxn)
    

    ## TODO: unhardcode

    ## If not specified, probability from i to j is 0
    probs_mat[, 1, 2] <-  params[1] * ode_results[1:T, 3] / N # S to I
    probs_mat[, 1, 1] <- 1 - probs_mat[, 1, 2] # S to S
    probs_mat[, 2, 3] <- params[2] # I to R
    probs_mat[, 2, 2] <- 1 - params[2] # I to I
    probs_mat[, 3, 3] <- 1 # R to R
           
    return(probs_mat)


}


#' Turn the base probs into individualized probs
#'
#' @param current_base_probs KxK matrix where entry ij is prob of transfer from i to j conditioned on current time step
#' @param current_agent_status integer vector of length N consisting of current status of agent
#' @return agent_probs N x K matrix where entry ni is probability of moving to state i conditioned on current state for agent n
base_to_agent_probs <- function(current_base_probs,
                                current_agent_status){
    agent_probs <- current_base_probs[current_agent_status,]
    return(agent_probs)

}
