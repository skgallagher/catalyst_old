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
