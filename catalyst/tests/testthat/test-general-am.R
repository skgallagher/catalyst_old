context("AM pairwise interactions")


test_that("Testing the infect susceptible from neighbors", {

    ## INITIAL PARAMS
    init_CM_vals <- c(1, 9, 0)
    N <- sum(init_CM_vals)
    K <- 3
    T <- 10
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
    env <- initialize_env(env_list, N, E)
    env_list$env_status <- env
    env_list$N <- N
####################################
    ## SIM LIST #################
##########################
    L <- 100
    sim_list <- list(L = L, do_parallel = FALSE)

##############################################33
    ## ACTUAL TESTS
###############################
    tt <- 0
    current_agent_status <- c(1, rep(2, 9))
    inf_nbr_inds <- c(2:10)
    disease_params_list <- list()
    disease_params_list$K <- 3
    disease_params_list$infection_states <- 2
    disease_params_list$susceptible_states <- 1
    disease_params_list$T <- T
    beta <- .5
    gamma <- .25
    params <- c(beta = beta, gamma = gamma)
    disease_params_list$params <- params
    disease_params_list$init_vals <- init_CM_vals
    base_probs <- initialize_probs(disease_params_list, SIR_fxn)
    disease_params_list$transmission_probs <- make_transmission_probs_SIR(beta, N, base_probs)

    ## infect_sus_from_nbrs
    infection_states <- 2
    current_sus_state <- 1
    new_states <- c(1, 1, 2, 1, 1, 1, 2, 1, 1)
    out <- determine_sus_infection(new_states, infection_states,
                                   current_sus_state)
    expect_equal(out, 2)
    ## Not infected
    new_states <- c(1,1, 1, 1)
    out <- determine_sus_infection(new_states, infection_states,
                                   current_sus_state)
    expect_equal(out, 1)
    
    ## infect_sus_from_nbr
    agent_prob <- infect_susceptible_from_nbrs(tt = 0,
                                               current_agent_status = current_agent_status,
                                               inf_nbr_inds = c(2:10),
                                               disease_params_list,
                                               current_sus_state = 1)
    ## Test to see if probs working
    B <- 1000
    agent_probs <- matrix(0, ncol =3, nrow = B)
    new_states <- numeric(B)
    for(bb in 1:B){
        agent_probs[bb,] <- infect_susceptible_from_nbrs(tt = 0,
                                               current_agent_status = c(1, rep(2, 9)),
                                               inf_nbr_inds = c(2:10),
                                               disease_params_list,
                                               current_sus_state = 1)
    }
    csums <- colSums(agent_probs) / B
    exp_inf <- beta * 9/10
    

####### RUN AM Step
    neighbor_list <- initialize_neighbors(env_list$env_status,
                                          env_list$N,
                                          env_list$E)
    out <- run_AM_step(tt=0, N = length(current_agent_status),
                       K = 3, current_agent_status = current_agent_status,
                       current_base_probs = base_probs[1,,],
                       neighbor_list = neighbor_list,
                       disease_params_list = disease_params_list,
                       agent_list = agent_list,
                       env_list = env_list)
    expect_equal(dim(out), c(N, 3))

    B <- 100
    out <- numeric(B)
    for(bb in 1:B){
        am <- run_AM_step(tt=0, N = length(current_agent_status),
                       K = 3, current_agent_status = current_agent_status,
                       current_base_probs = base_probs[1,,],
                       neighbor_list = neighbor_list,
                       disease_params_list = disease_params_list,
                       agent_list = agent_list,
                       env_list = env_list)
        out[bb] <- am[1, 2]
    }
    mean(out)
    
})
