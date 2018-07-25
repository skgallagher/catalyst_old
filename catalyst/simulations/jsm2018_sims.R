## SKG
## July 25, 2018
## Finally running some simulations!

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
    params <- c(beta = .1, gamma = .03)
    params_names <- c("beta", "gamma")
    infection_states <- c(2) # I is the infection state
    susceptible_states <- c(1) # S is the susceptible state
    transmission_probs <- matrix(c(1, 0, 0,
                                   1-beta, beta, 0,
                                   0, 0, 1),
                                 byrow = TRUE, ncol = 3)
    contact_probs = 1
    disease_params_list <- list(K = K,
                                infection_states = infection_states,
                                susceptible_states = susceptible_states,
                                init_vals = init_CM_vals,
                                params = params,
                                params_names = params_names,
                                T = T,
                                CM_fxn = SIR_fxn,
                                transmission_probs = transmission_probs,
                                contact_probs = contact_probs
                                )
########################################
    ## OUTPUT_PARAMS_LIST #################
##########################
    output_params_list <- list(do_write = TRUE,
                               save_sims = FALSE,
                               results_dir = "./jsm2018_sims",
                               base_name = "het0_results",
                               verbose = TRUE)
    
########################################
    ## do_AM #################
##########################
    do_AM <- FALSE
###################################
    
##########################################
## Actually RUN CATALYST  CM
##########################
time <- proc.time()[3]
cam_output <- catalyst(agent_list, env_list,
                       disease_params_list,
                       sim_list,
                       output_params_list,
                       do_AM = do_AM)
proc.time()[3] - time
###################################3

library(ggplot2)
library(reshape2)
library(dplyr)
library(latex2exp)
devtools::load_all("~/simCAM")
##########################################
## Plot plot plot
df <- read.csv("jsm2018_sims/het0_results_nstates.csv")
cols <- c("blue", "darkred", "darkgreen")
tex_symbol = c("\\hat{S}(t)", 
               "\\hat{I}(t)",
               "\\hat{R}(t)")
legend_names_tex <- lapply(tex_symbol, function(x){
        latex2exp::TeX(sprintf("%s", x))
})
df_melt <- melt(df, id.vars = c("ll", "tt"))
df_melt2 <- df_melt %>% group_by(tt, variable) %>%
    summarize(mu = mean(value),
              upper = quantile(value, .95),
              lower = quantile(value, .05))
#################################
ggplot(data = df_melt2, aes(x = tt)) + geom_line(aes(y = mu, col = variable)) + 
    labs(x = "Time", y = "# Individuals",
         title = "CM - Homogeneous Neighborhood",
         subtitle = latex2exp::TeX(sprintf("%d agents; %d runs; $\\beta = %.2f$; $\\gamma = %.2f$",
                                           N, L, disease_params_list$params[1],
                                           disease_params_list$params[2]))) +
    geom_ribbon(data = df_melt2, aes(ymin = lower, ymax = upper, fill = variable),
                alpha = .4) + 
    ggplot2::scale_color_manual(values = cols,
                                labels = legend_names_tex, name = "State") +
     ggplot2::scale_fill_manual(values = cols) + guides(fill = FALSE) + 
    my_theme()
