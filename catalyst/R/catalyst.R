## Main File to run primary catalyst functions
## SKG
## July 2, 2018
## To be integrated with current cpp code

catalyst <- function(agent_list, env_list,
                         disease_list,
                         sim_list,
                         run_AM = FALSE,
                         output_params_list){


    ## Imortant numbers
    N <- agent_list$N # number of agents
    T <- sim_list$T # total time steps, inclusive 0 to T
    K <- disease_list$K # total number of states
    E <- env_list$E # total number of environment TYPES (e.g. type is school and workplace not Center Twp Elementary and Butler High School)
    L <- sim_list$L # total number of runs
    
    ## Initialize Agents
    ## Move to RCPP
    agent_status <- initialize_agents(agent_list$init_CM_vals,
                                      N, K, T)
    

    ## Initialize environments
    ## Move to RCPP
    env_status <- initialize_env(env_list, N, E)

    ## Initialize Disease/CM
    ## May eventually want to RCPP
    CM_fxn <- make_CM_fxn(disease_list, N, K) # This is a function

    base_probs <- initialize_probs(disease_list, CM_fxn)
    
                    
    ## Set up neighbor dictionary/graph (best in CPP)
    neighbhor_list <- initialize_neighbors(env_status, N, E)
    
    ## Run the program
    ## Need to run some sections in rcpp, inner loop the bette
    catalyst_out <- run_cam(sim_list,
                            agent_status,
                            base_probs,
                            env_status,
                            CM_fxn,
                            neighbor_list,
                            output_params_list,
                            disease_params_list,
                            do_AM = FALSE)
    return(catalyst_out)
                             

}




run_cam <- function(sim_list,
                    agent_status,
                    base_probs,
                    env_status,
                    CM,
                    neighbor_list,
                    output_params_list,
                    disease_params_list,
                    do_AM = FALSE){

    ## TODO: DOUBLE CHECK INDEX
    agent_probs <- base_probs[1, ] ## Initialize the agent probabilities

    ## TODO: PARALLELIZE//CPP
    for(ll in 1:sim_list$L){ ## Number of runs
        output_list <- run_cam_inner(ll, agent_status,
                      agent_probs,
                      base_probs,
                      env_status,
                      neighbor_list,
                      output_params_list,
                      disease_params_list,
                      do_AM)
    }

    did_write <- write_output(output_list,
                              output_params_list, sim_list, CM)
                 
    return(output_list)



}

#' Inner part of running the CAM update
#'
#' @param ll simulation number
#' @param agent_status integer matrix of size (T+1)xN where entry nt is the state of agent n at time t-1
#' @param agent_probs NULL if do_AM is FALSE.  Else a NxK matrix where entry ni is prob of moving to state i conditioned on current state for agent n
#' @param base_probs T x K x K matrix where entry tij is the probability of an agent in state i moving to state j from time t to t+1
#' @param env_status a NxE matrix where entry ij is agent i's value assignment to environment j.  An assignment of 0 corresponds to a NULL assignment.  Agents with value 0 do not belong to the same environment
#' @param neighbor_list list of length N where entry n is a vector of indices of neighbors, that is people who share an environment assignment of non-zero for the same category.  The list entry has a 0 element if it has no neighbors
#' @param output_params_list list including "do_write" a logical,
#' "save_sims" a logical
#' "results_dir" a character string
#' "verbose" a logical
#' @param disease_params_list list including
#' "K" the number of states
#' "infection_states" the indices of the states that can infect others
#' "init_vals" vector of size K that sums to N, initial values in each of the states at time t=0
#' "params" vector of disease parameters (e.g. beta, gamma)
#' "params_names" optional vector of parameter names
#' T - total number of time steps, 0 to T-1 inclusive
#' CM_fxn - the compartment model function
#' @param do_AM logical.  Default is FALSE
#' @return a summarized list of the simulation
run_cam_inner <- function(ll, agent_status,
                      agent_probs,
                      base_probs,
                      env_status,
                      neighbor_list,
                      output_params_list,
                      disease_params_list,
                      do_AM = FALSE){
    ## TODO:  Function to do RCPP for
    T <- dim(base_probs)[1]
    for(tt in 0:(T-2)){
        if(do_AM){ # Run AM portion (individualized interactions)
            ## Return is modified agent_probs
            ## Prob of contact and prob of transmission to get infectious.  Need a vector of eligible states to transfer to (S to I subsystem and other system which may also change according to env, agent covariates)
            agent_probs <- run_AM_step(tt, N, K,
                                       agent_status[tt+1, ],
                                       agent_probs,
                                       base_probs[tt+1,,],
                                       env_status[tt+1,],
                                       neighbor_list,
                                       disease_params_list,
                                       agent_vars)
        } else{
            ## TODO: This can be improved in memory and probably time to update CMs
            agent_probs <- NULL
        }
        ## Update agents based on agent probabilities
        agent_status[tt + 2, ] <- update_agents(agent_probs,
                                                agent_status[tt + 1, ],
                                                base_probs[tt+1,,],
                                                do_AM)

        ## TODO
        ## Update Agent Probabilities
        base_probs <- update_base_probs(base_probs, tt)
        ## Update environments
        env_status <- update_env(env_status, tt)
    }

    ## Summarize current step
    cam_output <- summarize_cam(agent_status,
                                output_params_list,
                                ll, env_status,
                                disease_params_list,
                                do_AM)
    return(cam_output)
}
