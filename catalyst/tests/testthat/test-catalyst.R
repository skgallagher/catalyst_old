## SKG
## July 18, 2018

context("Testing the main function")

test_that("Setting everything up",{

    init_CM_vals <- c(950, 50, 0)
    N <- sum(init_CM_vals)
    K <- 3
    T <- 100
    L <- 1
    agents <- initialize_agents(init_CM_vals, N,
                                K, T)
    ## Enviroment
    tab <- table(rep(1, N)) # We are all neighbors on this blessed day
    env_list <- list()
    env_list$init_env_table <- tab
    env_list$N <- sum(tab)
    env_list$E <- length(tab)
    env <- initialize_env(env_list, N, E)
    env_list$env_status <- env
    ## Disease params
    params <- c(beta=.1, gamma = .03)
    params_names <- c("beta", "gamma")
    disease_params_list <- list(K = K, infection_states = c(2),
                         init_vals = init_CM_vals,
                         params = params,
                         params_names = params_names,
                         T = T,
                         times = 0:T,
                         CM_fxn = SIR_diff)
    sim_list <- list(L = L)
    run_AM <- FALSE
    output_params_list <- list(do_write = FALSE,
                               save_sims = FALSE,
                               results_dir = "~/catalyst_results",
                               verbose = TRUE)
    neighbor_list <- initialize_neighbors(env_list$env_status,
                                          env_list$N,
                                          env_list$E)
    base_probs <- initialize_probs(disease_params_list, SIR_diff)
                               
    ## base to agent_probs

    ## S to I and I to R
    current_base_probs <- matrix(c(0, 1, 0,
                                   0, 0, 1,
                                   0, 0, 1),
                                 byrow = TRUE, ncol = 3)    
    current_agent_status <- c(1, 1, 1, 2, 3)
    agent_probs <- base_to_agent_probs(current_base_probs,
                                       current_agent_status)
    expected_probs <- current_base_probs[current_agent_status,]
    expect_equal(agent_probs, expected_probs)
})


test_that("run_cam_inner() is working", {

    ## Setting up multitude of variables
    #######################################################
    ll <- 1
    init_CM_vals <- c(950, 50, 0)
    N <- sum(init_CM_vals)
    K <- 3
    T <- 100
    L <- 1
    agents <- initialize_agents(init_CM_vals, N,
                                K, T)
    agent_status <- agents
    ## Enviroment
    tab <- table(rep(1, N)) # We are all neighbors on this blessed day
    env_list <- list()
    env_list$init_env_table <- tab
    env_list$N <- sum(tab)
    env_list$E <- length(tab)
    env <- initialize_env(env_list, N, E)
    env_list$env_status <- env
    ## Disease params
    params <- c(beta=.1, gamma = .03)
    params_names <- c("beta", "gamma")
    disease_params_list <- list(K = K, infection_states = c(2),
                         init_vals = init_CM_vals,
                         params = params,
                         params_names = params_names,
                         T = T,
                         times = 0:T,
                         CM_fxn = SIR_diff)
    sim_list <- list(L = L)
    run_AM <- FALSE
    output_params_list <- list(do_write = FALSE,
                               save_sims = FALSE,
                               results_dir = "~/catalyst_results",
                               base_name = "sim_results",
                               verbose = TRUE)
    neighbor_list <- initialize_neighbors(env_list$env_status,
                                          env_list$N,
                                          env_list$E)
    base_probs <- initialize_probs(disease_params_list, SIR_diff)
    agent_probs <- NULL
    do_AM <- FALSE
##########################################
    cam_output <- run_cam_inner(ll, agent_status,
                      base_probs,
                      env_list$env_status,
                      neighbor_list,
                      output_params_list,
                      disease_params_list,
                      agent_list = NULL,
                      do_AM)
    expect_true(cam_output$ll == ll)
    expect_equal(dim(cam_output$n_states), c(T+1, K))
})


                            
                         






test_that("catalyst() works", {
#################################################
    ## Setting up multitude of variables
################################################
    ##

###########################################
###### AGENT_LIST#####################n
######################################
    init_CM_vals <- c(950, 50, 0)
    N <- sum(init_CM_vals)
    K <- 3
    T <- 100
    agent_list <- list(init_CM_vals = init_CM_vals,
                       N = N,
                       K = K,
                       T = T)
#########################################
    ## ENVIRONMENT ######################
#####################################
    tab <- table(rep(1, N)) # We are all neighbors on this blessed day
    env_list <- list()
    env_list$init_env_table <- tab
    env_list$E <- length(tab)
    env_list$N <- N

####################################
    ## SIM LIST #################
##########################
    L <- 10
    sim_list <- list(L = L, do_parallel = FALSE)
####################################
    
######################################
    ## DISEASE_PARAMS_LIST
#######################################
    params <- c(beta = .1, gamma = .03)
    params_names <- c("beta", "gamma")
    infection_states <- c(2) # I is the infection state
    disease_params_list <- list(K = K,
                                infection_states = infection_states,
                                init_vals = init_CM_vals,
                                params = params,
                                params_names = params_names,
                                T = T,
                                times = 0:T,
                                CM_fxn = SIR_diff)
########################################
    ## OUTPUT_PARAMS_LIST #################
##########################
    output_params_list <- list(do_write = FALSE,
                               save_sims = FALSE,
                               results_dir = "~/test_catalyst_results",
                               base_name = "sim_results",
                               verbose = TRUE)
    
########################################
    ## do_AM #################
##########################
    do_AM <- FALSE
###################################
    
##########################################
## Actually RUN CATALYST
##########################
    time <- proc.time()[3]
    cam_output <- catalyst(agent_list, env_list,
                     disease_params_list,
                     sim_list,
                     output_params_list,
                     do_AM = FALSE)
    proc.time()[3] - time
    expect_equal(length(cam_output), L)

#########################
    ## Try with plugin = TRUE
########################
    disease_params_list$do_plugin_probs <- TRUE
    base_probs <- initialize_probs(disease_params_list, SIR_diff)
    ##########################################
## Actually RUN CATALYST
##########################
    time <- proc.time()[3]
    cam_output <- catalyst(agent_list, env_list,
                     disease_params_list,
                     sim_list,
                     output_params_list,
                     do_AM = FALSE)
    proc.time()[3] - time
    expect_equal(length(cam_output), L)

    
    
})





## ## Calculation for total transmission prob to individual transmission prob
## x <- .4
## N <- 10
## y <- 1 - (1-x)^(1/N)
## B <- 1000
## is_infected <- integer(B)
## for(bb in 1:B){
##     z <- rbinom(N, 1, y)
##     if(sum(z) >= 1){
##         is_infected[bb] <- 1
##     }
## }
## mean(is_infected)


test_that("catalyst() works (do_AM = TRUE)", {
#################################################
    ## Setting up multitude of variables
################################################
    ##

###########################################
###### AGENT_LIST#####################n
######################################
    init_CM_vals <- c(950, 50, 0)
    N <- sum(init_CM_vals)
    K <- 3
    T <- 100
    agent_list <- list(init_CM_vals = init_CM_vals,
                       N = N,
                       K = K,
                       T = T)
#########################################
    ## ENVIRONMENT ######################
#####################################
    tab <- table(rep(1, N)) # We are all neighbors on this blessed day
    env_list <- list()
    env_list$init_env_table <- tab
    env_list$E <- length(tab)
    env_list$N <- N

####################################
    ## SIM LIST #################
##########################
    L <- 10
    sim_list <- list(L = L, do_parallel = FALSE)
####################################
    
######################################
    ## DISEASE_PARAMS_LIST
#######################################
    params <- c(beta = .1, gamma = .03)
    params_names <- c("beta", "gamma")
    infection_states <- c(2) # I is the infection state
    susceptible_states <- c(1) # S is the susceptible state
    probs <- matrix(c(1, 0, 0,
                      0, 1, 0,
                      0, 0, 1),
                    byrow = TRUE, ncol = 3)
    transmission_probs <- array(0, dim = c(T, 3, 3))
    transmission_probs[1:T,,] <- probs
    contact_probs = 1
    disease_params_list <- list(K = K,
                                infection_states = infection_states,
                                susceptible_states = susceptible_states,
                                init_vals = init_CM_vals,
                                params = params,
                                params_names = params_names,
                                T = T,
                                times = 0:T,
                                CM_fxn = SIR_diff,
                                transmission_probs = transmission_probs,
                                contact_probs = contact_probs
                                )
########################################
    ## OUTPUT_PARAMS_LIST #################
##########################
    output_params_list <- list(do_write = FALSE,
                               save_sims = FALSE,
                               results_dir = "~/test_catalyst_results",
                               base_name = "sim_results",
                               verbose = TRUE)
    
########################################
    ## do_AM #################
##########################
    do_AM <- TRUE
###################################
    
##########################################
## Actually RUN CATALYST
##########################
    time <- proc.time()[3]
    cam_output <- catalyst(agent_list, env_list,
                     disease_params_list,
                     sim_list,
                     output_params_list,
                     do_AM = do_AM)
    proc.time()[3] - time
    expect_equal(length(cam_output), L)
    ##  A one liner to extract the number of susceptibles in each simulation for each time step (minus the initial.  They should all get infected.
    ## This is not good code
    expect_true(all(sapply(lapply(cam_output, "[[", 1), "[", -1, 1) == 0))
})
