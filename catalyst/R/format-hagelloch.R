
#' Make an agent data frame from a data frame with dates of agents
#'
#' @param df original df. Based off the hagelloch.df from surveillance package
#' @param state_names names we are going to call the states
#' @param df_names what the state dates are currently called
#' @param state_levels integer values for the state_names
#' @return a T+1 x N matrix where T+1 is the final time step and N is the number of agents
make_agent_data <- function(df, state_names = c("S", "I", "R"), df_names = c("S", "I", "R"),
                            state_levels = 1:length(state_names)){
    T <- max(df[, df_names])
    N <- nrow(df)
    agent_data <- matrix(0, nrow = (T + 1), ncol = N)
    agent_data <- apply(hagelloch.df, 1, extract_times, df_names, state_levels, T+1)
    return(agent_data)
}


#' Turn the status dates into workable agent data
#'
#' @param row row of a data frame
#' @param df_names corresponding column names to find times of status
#' @param state_levels corresponding number to df_names
#' @param vec_length how long is vector
#' @return turn SIR data for hagelloch into agent formulation
extract_times <- function(row, df_names, state_levels, vec_length){
    vec <- integer(vec_length)
    init_date <- as.numeric(row[df_names[2]]) 
    vec[1:init_date] <- state_levels[1] ## pad through beginning
    start_ind <- init_date + 1
    if(length(df_names) > 2)
        for(ii in 2:(length(df_names) -1) ){
            date <- as.numeric(row[df_names[ii + 1]]) 
            vec[start_ind:date] <- state_levels[ii]
            start_ind <- date + 1
        }
    ## Pad end
    vec[start_ind:length(vec)] <- state_levels[length(state_levels)]
    

    return(vec)


}


#' Extract agent environment from data
#'
#' @param df based on hagelloch.df from library(surveillance)
#' @param env_vars which variables to use as neighborhood
#' @return a  N x E matrix where entry ne is the assignment of environment e for agent n
extract_agent_envs <- function(df, env_vars){
    env <- df %>% select(env_vars) %>% dplyr::mutate_at(vars(env_vars), as.integer) %>%
        dplyr::mutate_at(vars(env_vars), funs(replace(., is.na(.), 0)))
    return(env)
    

}
