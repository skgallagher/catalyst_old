## Base catalyst parameters
## SKG
## July 26, 2018 (Happy proposal anniversary)


library(testthat)
devtools::load_all("~/catalyst/catalyst")

## BASE SIMULATION
## N <- 1000
## beta <- .5
## gamma <- .25
## T <- 100
## L <- 100
## K <- 3
## CM_fxn <- SIR_fxn
## init_vals <- c(999, 1, 0)
## SIR MODEL



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
    L <- 100
    sim_list <- list(L = L, do_parallel = FALSE)
####################################
######################################
## DISEASE_PARAMS_LIST
#######################################
beta <- .5
gamma <- .25
params <- c(beta = beta, gamma = gamma)
params_names <- c("beta", "gamma")
infection_states <- c(2) # I is the infection state
susceptible_states <- c(1) # S is the susceptible state
## Probability of individual transmission
transmission_probs <- NULL
contact_probs = 1
disease_params_list <- list(K = K,
                            infection_states = infection_states,
                            susceptible_states = susceptible_states,
                            init_vals = init_CM_vals,
                            params = params,
                            params_names = params_names,
                            T = 30,
                            CM_fxn = SIR_fxn,
                            transmission_probs = transmission_probs,
                            contact_probs = contact_probs,
                            do_plugin_probs = FALSE
                            )
base_probs <- initialize_probs(disease_params_list, SIR_fxn)
transmission_probs <- make_transmission_probs_SIR(beta, N, base_probs)
contact_probs = 1
disease_params_list$transmission_probs = transmission_probs

