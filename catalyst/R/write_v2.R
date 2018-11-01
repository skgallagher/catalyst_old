
#' Write agent_data simulations to an (appended) .csv
#'
#' @param writing_dir directory where we will write out data
#' @param ll simulation number to be appended to agent data
#' @param agent_data T+1 x N matrix where T+1 is the final time step and N is the number of agents where entry tn = k means that agent n at time t is in state k.
#' @return logical
#' @details The written out put will be written within the writing_dir to a folder called "agent_data" and a file called "agent_data.csv".  The simulation number ll and time step (1 to nrow(agent_data) will be added to the FIRST TWO columns of the csv.  Assumes writing_dir exists
write_agent_data <- function(writing_dir, ll, agent_data){
    dn <- file.path(writing_dir, "agent_data")
    if(ll == 1){
        ## If file exists before we even start, we remove it and start fresh
        if(dir.exists(dn)) unlink(dn, recursive = TRUE)
        dir.create(dn)
           
    }
    ## Append the relevant info the df
    df <- data.frame(ll = rep(ll, nrow(agent_data)),
                              t = 1:nrow(agent_data))
    agent_df <- as.data.frame(agent_data)
    out_df <- cbind(df, agent_df)

    ## Write out
    fn <- file.path(dn, "agent_data.csv")
    data.table::fwrite(out_df, file = fn, append = TRUE, sep = ",")
    out <- TRUE

    return(out)
                    

}


#' Write initial parameters
#'
#' @param L total number of simulations to run.  Default is 1
#' @param ncores number of cores for doParallel.  Default is 1.
#' @param trans_fxn(t, agent_data, theta) a function that returns the KxK transition matrix where K is the number of states.  Entry ij is the POSTIVE number of agents moving from state i to state j from time t-1 to t for t=1, \dots, T.  Default is SI
#' @param theta disease parameters.  Default is c(beta) for basic SI model
#' @param agent_data a T+1 x N matrix where T+1 is the final time step and N is the number of agents where entry tn = k means that agent n at time t is in state k.
#' @param nbr_list of length N where entry n is a vector of indices of neighbors, that is people who share an environment assignment of non-zero for the same category.  The list entry has a 0 element if it has no neighbors
#' @param states numeric vector indicating the different states an agent may take.  Default is 1:2
#' @param inf_states subset of states that indicate the infectious states.  Default is 2.
#' @param sus_states subset of states that indicate the susceptible states.  Default is 1
#' @param sus_inf_arr a K x K x K array where entry ijk=1 means that an interaction between an agent in susceptible state i can put an agent in infectious state j into state k with non-zero probability. 
#' @param do_AM logical.  Default is TRUE, which runs the AM interactions
#' @param do_keep_agent_data logical indicating whether we should keep entire agent data. Default is TRUE
#' @param do_write_agent_data logical indicating whether we should write out agent_data. Default is FALSE
#' @param do_write_inits logical indicating whether we should write out initial values and parameters.  Default is FALSE.
#' @param writing_dir  Where we write out results to. Default is ".".
#' @return logical
#' @details writes initial parameters to a RDS file along with a timestamp
write_inits <- function(L,
                        ncores,
                        do_par,
                        trans_fxn,
                        theta,
                        agent_data,
                        nbr_list,
                        states,
                        inf_states,
                        sus_states,
                        sus_inf_arr,
                        do_AM,
                        do_keep_agent_data,
                        do_write_agent_data,
                        do_write_inits,
                        writing_dir){

    ## TODO: do better than saving entire agent data and neighbor list
    time <- Sys.time()
    dn <- file.path(writing_dir, "log")
    if(!dir.exists(dn)) dir.create(dn)
    fn <- file.path(dn, "logfile.Rda")
    save(L, ncores, do_par, trans_fxn,
         theta,
         agent_data,
         nbr_list,
         states,
         inf_states,
         sus_states,
         sus_inf_arr,
         do_AM,
         do_keep_agent_data,
         do_write_agent_data,
         do_write_inits,
         writing_dir, file= fn)
         
    return(TRUE)
}
