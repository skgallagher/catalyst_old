## SKG
## July 18, 2018
## Get the summaries and write the outputs
## In a way Bill will be proud - replicable and stuff


#' Summarize the simulation
#'
#' @param agent_status integer matrix of (T+1)xN where entry tn is status of agent n at time t-1, taking values 1 through K
#' @param output_params_list list including "do_write" a logical,
#' "save_sims" a logical
#' "results_dir" a character string
#' "verbose" a logical
#' @param ll simulation number
#' @param env_status
#' @param disease_params_list
#' list including
#' "K" the number of states
#' "infection_states" the indices of the states that can infect others
#' "init_vals" vector of size K that sums to N, initial values in each of the states at time t=0
#' "params" vector of disease parameters (e.g. beta, gamma)
#' "params_names" optional vector of parameter names
#' T - total number of time steps, 0 to T-1 inclusive
#' CM_fxn - the compartment model function
#' @param do_AM a logical
#' @return list with
#' "n_states" a TxK matrix where entry ti is the number of agents at time t-1 in state i
#' "ll" simulation number
#' "do_AM" a logical
summarize_cam <- function(agent_status,
                          output_params_list,
                          ll,
                          env_status,
                          disease_params_list,
                          do_AM){
    K <- disease_params_list$K
    n_states <- t(apply(agent_status, 1, function(row){
        row_factors <- factor(row, levels = 1:K)
        return(table(row_factors))
        
    }))
    names(n_states) <- NULL

    ## TODO: more sophisticated outputs
       
    return(list(n_states = n_states,
                ll = ll,
                do_AM = do_AM))

}


#' Function to write outputs
#'
#' @param output_list list with
#' "n_states" a TxK matrix where entry ti is the number of agents at time t-1 in state i
#' "ll" simulation number
#' "do_AM" a logical
#' @param output_params_list list including "do_write" a logical,
#' "save_sims" a logical
#' "results_dir" a character string
#' "verbose" a logical
#' @param sim_list list including "T" max amount of time steps, 0 to T inclusive
#' "L" - total number of simulations
#' @param disease_params_list list including
#' "K" the number of states
#' "infection_states" the indices of the states that can infect others
#' "init_vals" vector of size K that sums to N, initial values in each of the states at time t=0
#' "params" vector of disease parameters (e.g. beta, gamma)
#' "params_names" optional vector of parameter names
#' T - total number of time steps, 0 to T-1 inclusive
#' CM_fxn - the compartment model function
#' @param agent_list list including
#' "N" total number of agents
#' "init_CM_vals" - vector of initial state sof the compartments
#' @param env_list  list with
#' "init_env_table" a contingency table of number of individuals in each each cross section.  Each dimension name of the table corresponds to a number 1 to E_j, the number of total specific elements in Environment j.
#' "E" total types of environments
#' @param run_AM logical
write_output <- function(output_list,
                         output_params_list,
                         sim_list,
                         disease_params_list,
                         agent_list,
                         env_list,
                         do_AM){

    do_write <- output_params_list$do_write
    verbose <- output_params_list$verbose
    results_dir <- output_params_list$results_dir
    base_name <- output_params_list$base_name
    
    if(!do_write){
        print.verbose(verbose,
                      str = "Returning output without saving.")
    } else {
        print.verbose(verbose,
                      str = paste0("Checking if ", results_dir,
                                   " exists."))
        if(!dir.exists(results_dir)){
            print.verbose(verbose,
                          str = paste0("Creating directory: ",
                                       results_dir))
            dir.create(results_dir, recursive = TRUE)
        }
        n_states_df <- make_n_states_df(output_list)
        df_path <- paste0(base_name, "_nstates.csv")
        ## TODO: Possibly change to fwrite
        print.verbose(verbose,
                      str = paste0("Writing n_states to ", df_path))
        write.csv(n_states_df, file.path(results_dir, df_path),
                  row.names = FALSE)
        ## And the rest of the params
        input_path <- paste0(base_name, "_run_params.RDS")
        run_parameters <- list(output_params_list = output_params_list,
                         sim_list = sim_list,
                         disease_params_list = disease_params_list,
                         agent_list = agent_list,
                         env_list = env_list,
                         do_AM = do_AM)
        print.verbose(verbose,
                      str = paste0("Writing run parameters to ", input_path))
        saveRDS(run_parameters, file.path(results_dir, input_path))
    }
    return(TRUE)
}

#' Print if verbose is TRUE
#'
#' @param verbose logical
#' @param str character string to be printed
#' @return TRUE
print.verbose <- function(verbose, str){
    if(verbose) print(str)
    return(TRUE)
}


#' Make data frame of total number of individuals
#'
#' @param output_list list with
#' "n_states" a (T+1)xK matrix where entry ti is the number of agents at time t-1 in state i
#' "ll" simulation number
#' "do_AM" a logical
make_n_states_df <- function(output_list){
    ## TODO: speed up from rbind
    df <- do.call('rbind',
                  lapply(1:length(output_list),
                         function(ll){
                             df <- output_list[[ll]]$n_states
                             new_df <- data.frame(ll = ll,
                                                  tt = 0:(nrow(df) -1),
                                                  df)
                             return(new_df)
                         }
                   ))

    
    }
