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
    CM_fxn<- make_CM_fxn(disease_list, N, K) # This is a function

    base_probs <- initialize_probs(disease_list, CM_fxn)
    
                    
    ## Set up neighbor dictionary/graph (best in CPP)
    neighbhor_list <- initialize_neighbors(env_status, N, E)
    
    ## Run the program
    ## Need to run some sections in rcpp, inner loop the bette
    catalyst_out <- run_cam(sim_list,
                             agent_status,
                             base_probs,
                             env_status,
                             CM,
                             neighbor_list,
                             output_params_list)
    return(catalyst_out)
                             

}




run_cam <- function(sim_list,
                    agent_status,
                    base_probs,
                    env_status,
                    CM,
                    neighbor_list,
                    output_params_list){

    ## TODO: DOUBLE CHECK INDEX
    agent_probs <- base_probs[1, ] ## Initialize the agent probabilities

    ## TODO: PARALLELIZE//CPP
    for(ll in 1:sim_list$L){ ## Number of runs
        output_list <- run_cam_inner(ll, agent_status,
                      agent_probs,
                      base_probs,
                      env_status,
                      CM,
                      neighbor_list,
                      output_params_list)
    }

    write_output(output_list, output_params_list, sim_list, CM)
                 
    return(output_list)



}

## Function to do RCPP for
run_cam_inner <- function(ll, agent_status,
                      agent_probs,
                      base_probs,
                      env_status,
                      CM,
                      neighbor_list,
                      output_params_list){

    for(tt in 0:(T-1)){
        if(do_AM){ # Run AM portion (individualized interactions)
            ## Return is modified agent_probs
            ## Prob of contact and prob of transmission to get infectious.  Need a vector of eligible states to transfer to (S to I subsystem and other system which may also change according to env, agent covariates)
            agent_probs <- run_AM_step(tt, N, K,
                                       agent_status,
                                       base_probs,
                                       env_status,
                                       neighbor_list,
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
    cam_output <- summarize_cam(agent_status, output_params_list, ll)
    return(cam_output)
}
