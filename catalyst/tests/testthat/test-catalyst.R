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
                         CM_fxn = SIR_fxn)
    sim_list <- list(L = L)
    run_AM <- FALSE
    output_params_list <- list(do_write = FALSE,
                               save_sims = FALSE,
                               results_dir = "~/catalyst_results",
                               verbose = TRUE)
    neighbor_list <- initialize_neighbors(env_list$env_status,
                                          env_list$N,
                                          env_list$E)
    base_probs <- initialize_probs(disease_params_list, CM_fxn)
                               
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
                         CM_fxn = SIR_fxn)
    sim_list <- list(L = L)
    run_AM <- FALSE
    output_params_list <- list(do_write = FALSE,
                               save_sims = FALSE,
                               results_dir = "~/catalyst_results",
                               verbose = TRUE)
    neighbor_list <- initialize_neighbors(env_list$env_status,
                                          env_list$N,
                                          env_list$E)
    base_probs <- initialize_probs(disease_list, CM_fxn)
    agent_probs <- NULL
    do_AM <- FALSE
##########################################
    cam_output <- run_cam_inner(ll, agent_status,
                      agent_probs,
                      base_probs,
                      env_status,
                      neighbor_list,
                      output_params_list,
                      disease_params_list,
                      do_AM)
    expect_true(cam_output$ll == ll)
    expect_equal(dim(cam_output$n_states), c(T+1, K))
})


                            
                         




