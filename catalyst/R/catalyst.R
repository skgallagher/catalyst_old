## Main File to run primary catalyst functions
## SKG
## July 2, 2018
## To be integrated with current cpp code


#' Run the compartment-agent-based model
#' 
#' @param agent_list list with
#' "init_CM_vals" vector of size K where entry k is the size of state k at time 0. 
#' "N" number of total agents
#' "K" number of total disease states
#' "T" max time, integer value
#' @param env_list list with
#' "init_env_table" a contingency table of number of individuals in each each cross section.  Each dimension name of the table corresponds to a number 1 to E_j, the number of total specific elements in Environment j
#' "E" total number of environment types (e.g. school and workplace is 2)
#' "N" total number of agents
#' @param sim_list list with
#' "L" total number of simulations to run
#' "do_parallel" logical
#' @param disease_params_list list including
#' "K" the number of states
#' "infection_states" the indices of the states that can infect others
#' "susceptible_states" the indices of the states that are susceptible
#' "init_CM_vals" vector of size K that sums to N, initial values in each of the states at time t=0
#' "params" vector of disease parameters (e.g. beta, gamma)
#' "params_names" optional vector of parameter names
#' T - total number of time steps, 0 to T-1 inclusive
#' CM_fxn - the compartment model function
#' "transmission_probs" n_infectious_states x K where entry ij is prob of agent in infectious state i agent to state j.  NOTE: n_infectious_states are those that CAN transmit the disease.  
#' "contact_probs" probability of contact with another agent
#' @param output_params_list list including "do_write" a logical,
#' "save_sims" a logical
#' "results_dir" a character string
#' "verbose" a logical
#' @param do_AM logical.  Default is FALSE
#' @return a summarized list of the simulation
#' @export
catalyst <- function(agent_list, env_list,
                     disease_params_list,
                     sim_list,
                     output_params_list,
                     do_AM = FALSE){


    ## Imortant numbers to extract
    N <- agent_list$N # number of agents
    T <- disease_params_list$T # total time steps, inclusive 0 to T
    K <- disease_params_list$K # total number of states
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
    CM_fxn <- make_CM_fxn(disease_params_list, N, K) # This is a function
    disease_params_list$CM_fxn <- CM_fxn

    base_probs <- initialize_probs(disease_params_list, CM_fxn)
    
                    
    ## Set up neighbor dictionary/graph (best in CPP)
    neighbhor_list <- initialize_neighbors(env_status, N, E)
    
    ## Run the program
    ## Need to run some sections in rcpp, inner loop the bette
    catalyst_out <- run_cam(sim_list,
                            agent_status,
                            base_probs,
                            env_status,
                            neighbor_list,
                            output_params_list,
                            disease_params_list,
                            agent_list,
                            do_AM = FALSE)
    return(catalyst_out)
                             

}



#' Run the compartment-agent-based model
#'
#' @param sim_list list with
#' "T" total amount of time 0 to T inclusive
#' "L" total number of simulations to run
#' "do_parallel" logical
#' @param agent_status integer matrix of size (T+1)xN where entry nt is the state of agent n at time t-1
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
#' "susceptible_states" the indices of the states that are susceptible
#' "init_vals" vector of size K that sums to N, initial values in each of the states at time t=0
#' "params" vector of disease parameters (e.g. beta, gamma)
#' "params_names" optional vector of parameter names
#' T - total number of time steps, 0 to T-1 inclusive
#' CM_fxn - the compartment model function
#' @param agent_list list with
#' "init_CM_vals" vector of size K where entry k is the size of state k at time 0. 
#' "N" number of total agents
#' "K" number of total disease states
#' "T" max time, integer value
#' @param do_AM logical.  Default is FALSE
#' @return a summarized list of the simulation#' Run the compartment-agent-based model
run_cam <- function(sim_list,
                    agent_status,
                    base_probs,
                    env_status,
                    neighbor_list,
                    output_params_list,
                    disease_params_list,
                    agent_list,
                    do_AM = FALSE){



    cam_output <- vector(mode = "list", length = sim_list$L)
    ## TODO: PARALLELIZE//CPP
    for(ll in 1:sim_list$L){ ## Number of runs
        cam_output[[ll]] <- run_cam_inner(ll, agent_status,
                                     base_probs,
                                     env_status,
                                     neighbor_list,
                                     output_params_list,
                                     disease_params_list,
                                     agent_list,
                                     do_AM)
    }

    did_write <- write_output(cam_output,
                              output_params_list, disease_params_list, CM)
                 
    return(cam_output)



}

#' Inner part of running the CAM update
#'
#' @param ll simulation number
#' @param agent_status integer matrix of size (T+1)xN where entry nt is the state of agent n at time t-1
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
#' "susceptible_states" the indices of the states that are susceptible
#' "init_vals" vector of size K that sums to N, initial values in each of the states at time t=0
#' "params" vector of disease parameters (e.g. beta, gamma)
#' "params_names" optional vector of parameter names
#' T - total number of time steps, 0 to T-1 inclusive
#' CM_fxn - the compartment model function
#' "transmission_probs" n_infectious_states x n_infection_states where entry ij is prob of agent in infectious state i agent to state j.  NOTE: n_infectious_states are those that CAN transmit the disease.  n_infection_states can include latently infectious individuals
#' "contact_probs" probability of contact with another agent
#' @param agent_list list with
#' "init_CM_vals" vector of size K where entry k is the size of state k at time 0. 
#' "N" number of total agents
#' "K" number of total disease states
#' "T" max time, integer value
#' "agent_vars" covariates of agents 
#' @param do_AM logical.  Default is FALSE
#' @return a summarized list of the simulation
run_cam_inner <- function(ll, agent_status,
                      base_probs,
                      env_status,
                      neighbor_list,
                      output_params_list,
                      disease_params_list,
                      agent_list,
                      do_AM = FALSE){
    ## TODO:  Function to do RCPP for
    T <- dim(base_probs)[1]
    for(tt in 0:(T-1)){
        if(do_AM){ # Run AM portion (individualized interactions)
            ## Return is modified agent_probs
            ## Prob of contact and prob of transmission to get infectious.  Need a vector of eligible states to transfer to (S to I subsystem and other system which may also change according to env, agent covariates)
            agent_probs <- run_AM_step(tt, N, K,
                                       agent_status[tt+1, ],
                                       base_probs[tt+1,,],
                                       neighbor_list,
                                       disease_params_list,
                                       agent_list,
                                       env_list)
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
