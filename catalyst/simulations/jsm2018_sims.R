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
beta <- .5
gamma <- .25
params <- c(beta = beta, gamma = gamma)
params_names <- c("beta", "gamma")
infection_states <- c(2) # I is the infection state
susceptible_states <- c(1) # S is the susceptible state
## Probability of individual transmission
prob_ind_trans <- 1 - (1 - beta)^(1/N)
transmission_probs <- matrix(c(1, 0, 0,
                               1 - prob_ind_trans, prob_ind_trans, 0,
                               0, 0, 1),
                             byrow = TRUE, ncol = 3)
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
                            contact_probs = contact_probs
                            )
########################################
## OUTPUT_PARAMS_LIST #################
##########################
output_params_list <- list(do_write = TRUE,
                           save_sims = FALSE,
                           results_dir = "./jsm2018_sims",
                           base_name = "het0_results_CM",
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
type <- ifelse(do_AM, "AM", "CM")
df <- read.csv(paste0("jsm2018_sims/",
                     output_params_list$base_name,
                      "_nstates.csv"))
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
         title = paste0(type, " - Homogeneous Neighborhood"),
         subtitle = latex2exp::TeX(sprintf("%d individuals; %d runs; $\\beta = %.2f$; $\\gamma = %.2f$",
                                           N, L, disease_params_list$params[1],
                                           disease_params_list$params[2]))) +
    geom_ribbon(data = df_melt2, aes(ymin = lower, ymax = upper, fill = variable),
                alpha = .4) + 
    ggplot2::scale_color_manual(values = cols,
                                labels = legend_names_tex, name = "State") +
     ggplot2::scale_fill_manual(values = cols) + guides(fill = FALSE) + 
    my_theme()
    ggsave(paste0("plots/",
                  output_params_list$base_name,
                  "_", 
                  ".pdf"), width = 8, height = 6)


####################################################################

##########################################
## Actually RUN CATALYST  AM
##########################
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
prob_ind_trans <- 1 - (1 - beta)^(1/N)
transmission_probs <- matrix(c(1, 0, 0,
                               1 - prob_ind_trans, prob_ind_trans, 0,
                               0, 0, 1),
                             byrow = TRUE, ncol = 3)
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
                            contact_probs = contact_probs
                            )
########################################
## OUTPUT_PARAMS_LIST #################
##########################
devtools::load_all("~/catalyst/catalyst")
output_params_list <- list(do_write = TRUE,
                           save_sims = FALSE,
                           results_dir = "./jsm2018_sims",
                           base_name = "het0_results_AM",
                           verbose = TRUE)
####################################
    ## SIM LIST #################
##########################
    L <- 100
    sim_list <- list(L = L, do_parallel = FALSE)
####################################
## DO AM
#########################
do_AM <- TRUE
########################################
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
type <- ifelse(do_AM, "AM", "CM")
df <- read.csv(paste0("jsm2018_sims/",
                     output_params_list$base_name,
                      "_nstates.csv"))
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
         title = paste0(type, " - Homogeneous Neighborhood"),
         subtitle = latex2exp::TeX(sprintf("%d individuals; %d runs; $\\beta = %.2f$; $\\gamma = %.2f$",
                                           N, L, disease_params_list$params[1],
                                           disease_params_list$params[2]))) +
    geom_ribbon(data = df_melt2, aes(ymin = lower, ymax = upper, fill = variable),
                alpha = .4) + 
    ggplot2::scale_color_manual(values = cols,
                                labels = legend_names_tex, name = "State") +
     ggplot2::scale_fill_manual(values = cols) + guides(fill = FALSE) + 
     my_theme()
    ggsave(paste0("plots/",
                  output_params_list$base_name,
                  "_", 
                  ".pdf"), width = 8, height = 6)




##############################################################
## Get the likelihood
#########################################################

devtools::load_all("~/catalyst/catalyst")


########################################################3
init_params <- c(beta=.5, gamma=.25)
do_plug_in <- FALSE ## Run the ODE SIR
disease_list <- list(params = init_params, params_names = NULL,
                     T = max(df$tt), init_vals = unlist(df[1, c("X1", "X2", "X3")]))
optim_pars <- optim(par = init_params,
                    fn = loglike_sir, data = df, disease_list = disease_list,
                    do_plug_in = do_plug_in)
optim_pars


out <- loglike_sir(optim_pars, new_df)
