



########################################
## OUTPUT_PARAMS_LIST #################
##########################
source("catalyst_inputs.R")
source("plot.R")
x <- sqrt(N)
y <- sqrt(N)
do_AM <- TRUE
type <- ifelse(do_AM, "AM", "CM")
base_name <- paste0("x-", x^2 / N, "_y-", y^2 / N, "_results_", type)
output_params_list <- list(do_write = TRUE,
                           save_sims = FALSE,
                           results_dir = "./jsm2018_sims",
                           base_name = base_name,
                           verbose = TRUE)

### RUN
##########################################
## Actually RUN CATALYST  AM
##########################
time <- proc.time()[3]
cam_output <- catalyst(agent_list, env_list,
                       disease_params_list,
                       sim_list, 
                       output_params_list,
                       do_AM = do_AM)
t2 <- proc.time()[3] - time
t2

df_AM <- read.csv(paste0("jsm2018_sims/",
                     output_params_list$base_name,
                      "_nstates.csv"))

## Plot
g_AM <- plot_sir(df_AM) +  theme(legend.position="bottom") 
g_AM

file_name <- paste0("sir-", "homogeneous-AM", ".pdf")
ggsave(file.path("../plots/", file_name),
       width = 10, height=8)    




######################################
## Now optimize likelihood using plug-
######################################
init_params <- c(beta=.5, gamma=.25)
disease_list_AM <- list(params = init_params, params_names = NULL,
                     T = max(df_AM$tt), init_vals = unlist(df_AM[1, c("X1", "X2", "X3")]))
do_plug_in <- TRUE ## Do run the ODE SIR
optim_pars_AM <- optim(par = init_params,
                    fn = loglike_sir, data = df_AM, disease_list = disease_list_AM,
                    do_plug_in = do_plug_in)
## optim_pars


##############
## Now simulate CM
###############
do_AM <- FALSE
type <- ifelse(do_AM, "AM", "CM")
base_name <- paste0("x-", x^2 / N, "_y-", y^2 / N, "_results_", type)
output_params_list <- list(do_write = TRUE,
                           save_sims = FALSE,
                           results_dir = "./jsm2018_sims",
                           base_name = base_name,
                           verbose = TRUE)
disease_params_list$params <- optim_pars_AM$par
disease_params_list$do_plugin_probs <- TRUE ## should get wider var

### RUN
##########################################
## Actually RUN CATALYST  CM
##########################
time <- proc.time()[3]
cam_output <- catalyst(agent_list, env_list,
                       disease_params_list,
                       sim_list,
                       output_params_list,
                       do_AM = do_AM)
t2 <- proc.time()[3] - time
t2

df_CM <- read.csv(paste0("jsm2018_sims/",
                     output_params_list$base_name,
                     "_nstates.csv"))


## Plot
g_CM <- plot_sir(df_CM)  + theme(legend.position="bottom" )
g_CM
file_name <- paste0("sir-", "homogeneous-CM", ".pdf")
ggsave(file.path("../plots/", file_name),
       width = 10, height=8)    


library(gridExtra)
grid.arrange(g_AM, g_CM)

######################################
## Now optimize likelihood using plug-
######################################
init_params <- c(beta=.5, gamma=.25)
disease_list_CM<- list(params = init_params, params_names = NULL,
                     T = max(df_CM$tt), init_vals = unlist(df_CM[1, c("X1", "X2", "X3")]))
do_plug_in <- TRUE ## Don't run the ODE SIR
optim_pars_CM <- optim(par = init_params,
                    fn = loglike_sir, data = df_CM, disease_list = disease_list_CM,
                    do_plug_in = do_plug_in)
## optim_pars


## get log likes!

rbind(optim_pars_AM$par, optim_pars_CM$par)

library(plyr)
loglike_AM <- ddply(.data = df_AM, .var = c("ll"),
                          .fun = function(df){
                              loglike_sir(
                                  params = optim_pars_CM$par,
                                  data = df,
                                  disease_list = disease_list_AM,
                                  do_plug_in = do_plug_in
                              )
                          })

loglike_CM <- ddply(.data = df_CM, .var = c("ll"),
                          .fun = function(df){
                              loglike_sir(
                                  params = optim_pars_CM$par,
                                  data = df,
                                  disease_list = disease_list_AM,
                                  do_plug_in = do_plug_in
                              )
                          })
                          

cols <- c("cornflowerblue", "orange1")
df_ll <- data.frame(Simulation = loglike_AM$ll,
                    loglike_AM = loglike_AM$V1,
                    loglike_CM = loglike_CM$V1)
df_ll_melt <- melt(df_ll, id.vars = "Simulation")


###############################3
## density estimates
g_de <- ggplot(df_ll_melt, aes(x = -value, col=variable)) +
    geom_density(size =2) + 
    labs(x = "Log Likelihood", y = "Count",
         title = TeX(sprintf("Density Estimates of Log Likelihoods: $x=\\sqrt{N}$, $y=\\sqrt{N}$")),
         subtitle = latex2exp::TeX(sprintf("%d individuals; %d runs; $\\beta = %.2f$; $\\gamma = %.2f$; Prob. Transmission =  $1-(1-\\beta)^{1/X_2}$",   N, L, disease_list_AM$params[1],
                                           disease_list_AM$params[2]))) +
    geom_rug(aes(y=0)) + 
    ggplot2::scale_colour_manual(values = cols, name = "Type", labels = c("AM", "CM"))+
    my_theme() +     theme(legend.position="bottom")  + xlim(-200, -140)
g_de

file_name <- paste0("loglike_de_x-", "homogeneous", ".pdf")
ggsave(file.path("../plots/", file_name),
       width = 9, height=10)    
