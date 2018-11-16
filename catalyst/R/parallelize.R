#' Run multiple simulations of catalyst
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
#' @param catalyze_fxn Default is catalyze.  Other option is catalyze_am_si.  See details
#' @return list of simulations
#' @details TODO
#' @importFrom foreach %dopar%
#' @importFrom foreach %do%
simulate_catalyst <- function(L = 1,
                              ncores = 1,
                              do_par = FALSE,
                              trans_fxn = SI,
                              theta,
                              agent_data,
                              nbr_list,
                              states = 1:2,
                              inf_states = 2,
                              sus_states = 1,
                              sus_inf_arr,
                              do_AM = TRUE,
                              do_keep_agent_data = TRUE,
                              do_write_agent_data = FALSE,
                              do_write_inits = FALSE,
                              writing_dir = ".",
                              catalyze_fxn = catalyze){


    if(do_write_inits | do_write_agent_data){
        if(!dir.exists(writing_dir)) dir.create(writing_dir,
                                                recursive = TRUE)

    }

        if(do_write_inits){ ## Write initial parameters
            write_inits(L = L,
                        ncores = ncores,
                        do_par = do_par,
                        trans_fxn = trans_fxn,
                        theta = theta,
                        agent_data = agent_data,
                        nbr_list = nbr_list,
                        states = states,
                        inf_states = inf_states,
                        sus_states = sus_states,
                        sus_inf_arr = sus_inf_arr,
                        do_AM = do_AM,
                        do_keep_agent_data = do_keep_agent_data,
                        do_write_agent_data = do_write_agent_data,
                        do_write_inits = do_write_inits,
                        writing_dir = writing_dir,
                        catalyze_fxn = ctalyze_fxn)
        }
    
    if(!do_par){  ## just do a %do% loop with foreach
        sims <- foreach::foreach(ll=1:L,
                                 .packages = "foreach") %do% { 
                                     out <- catalyze_fxn(ll,
                                                     trans_fxn = trans_fxn,
                                                     theta = theta,
                                                     agent_data = agent_data,
                                                     nbr_list = nbr_list,
                                                     states = states,
                                                     inf_states = inf_states,
                                                     sus_states = sus_states,
                                                     sus_inf_arr = sus_inf_arr,
                                                     do_AM = do_AM,
                                                     do_keep_agent_data = do_keep_agent_data,
                                                     do_write_agent_data = do_write_agent_data,
                                                     writing_dir = writing_dir)
                                     
                                 }

        
        return(sims)

    } else {  ## Use %dopar%


    }



}

