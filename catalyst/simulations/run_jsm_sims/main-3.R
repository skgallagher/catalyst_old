



### Differing homogeneity, even split
########################################
## OUTPUT_PARAMS_LIST #################
##########################
source("catalyst_inputs.R")
source("plot.R")
x <- sqrt(N) / 4
y <- sqrt(N) / 3
n <- sqrt(N)
mat <- matrix(0, nrow = 2, ncol = 2)
mat[1, 1] <- floor(x * (n - y))
mat[2, 2] <- floor((n-x) * (y))
mat[2, 1] <- floor(x * (y))
mat[1, 2] <- N - mat[1,1] - mat[2, 2] - mat[2, 1]
tab <- as.table(mat)
colnames(tab) <- c(1, 2)
rownames(tab) <- c(1, 2)
env_list$E <- 2
env_list$init_env_table <- tab
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
g_AM <- plot_sir(df_AM)
g_AM

library(gridExtra)


######################################
## Now optimize likelihood using plug-
######################################
init_params <- c(beta=.5, gamma=.25)
disease_list_AM <- list(params = init_params, params_names = NULL,
                     T = max(df_AM$tt), init_vals = unlist(df_AM[1, c("X1", "X2", "X3")]))
do_plug_in <- TRUE ## Don't run the ODE SIR
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
g_CM <- plot_sir(df_CM)
g_CM

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

library(plyr)
loglike_AM <- ddply(.data = df_AM, .var = c("ll"),
                          .fun = function(df){
                              loglike_sir(
                                  params = optim_pars_AM$par,
                                  data = df,
                                  disease_list = disease_list_CM,
                                  do_plug_in = do_plug_in
                              )
                          })

loglike_CM <- ddply(.data = df_CM, .var = c("ll"),
                          .fun = function(df){
                              loglike_sir(
                                  params = optim_pars_AM$par,
                                  data = df,
                                  disease_list = disease_list_CM,
                                  do_plug_in = do_plug_in
                              )
                          })
                          
par(mfrow=c(2,1))
hist(loglike_AM$V1)
hist(loglike_CM$V1)

cols <- c("midnightblue", "orange1")
df_ll <- data.frame(Simulation = loglike_AM$ll,
                    loglike_AM = loglike_AM$V1,
                    loglike_CM = loglike_CM$V1)
df_ll_melt <- melt(df_ll, id.vars = "Simulation")
mu <- ddply(df_ll_melt, "variable", summarize, type_mean = mean(-value))
g_hist <- ggplot(df_ll_melt, aes(x = -value, fill=variable)) +
    geom_histogram(position = "identity", alpha = .5) + my_theme() +
    labs(x = "Log Likelihood", y = "Count",
         title = TeX(sprintf("Histograms of Log Likelihoods: $x=\\sqrt{N}$, $y=\\sqrt{N}$")),
         subtitle = latex2exp::TeX(sprintf("%d individuals; %d runs; $\\beta = %.2f$; $\\gamma = %.2f$; Prob. Transmission =  $1-(1-\\beta)^{1/N}$",   N, L, disease_list_AM$params[1],
                                           disease_list_AM$params[2]))) +
    ggplot2::scale_fill_manual(values = cols, name = "Type",  labels = c("AM", "CM"))+
    geom_vline(data = mu, aes(xintercept = type_mean, color=variable, group = variable),
               linetype="dashed") +
    ggplot2::scale_color_manual(values = cols) + guides(color = FALSE) +
    theme(legend.position="bottom")
g_hist
file_name <- paste0("loglike_hist_x-", round(10 * x / sqrt(N)),
                    "_y-", round(10 * y / sqrt(N)),
                    ".pdf")
ggsave(file.path("../plots/", file_name),
       width = 9, height=10)    
