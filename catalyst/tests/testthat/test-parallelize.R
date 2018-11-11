context("parallel stuff")

test_that("Simulate catalyst", {

    ## SI
    L <- 10
    ncores <- 1
    do_par <- FALSE
    trans_fxn <- SI
    theta <- .4
    N <- 100
    T <- 20
    K <- 2
    agent_data <- matrix(1, nrow = T, ncol = N)
    agent_data[1, 91:100] <- 2
    nbr_list <- rep(list(1:N), N)
    states <- 1:2
    sus_states <- 1
    inf_states <- 2
    sus_inf_arr <- array(0, dim = c(K, K, K))
    sus_inf_arr[1, 2, ] <- c(1 - theta[1]/ N, theta[1]/ N)
    do_AM <- FALSE
    do_keep_agent_data <- TRUE
    do_write_agent_data <- TRUE
    do_write_inits <- FALSE
    writing_dir <- "./tmp"


    out <- simulate_catalyst(L = L,
                             ncores = ncores,
                             do_par = do_par,
                             trans_fxn = SI,
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
                             writing_dir = writing_dir)


    expect_equal(length(out), L)

    unlink(writing_dir, recursive = TRUE)

    X_df <- extract_X_df(out)

 

    df_plot <- X_df %>% dplyr::group_by(tt) %>%
        select(-c("ll")) %>%
        dplyr::summarize_all(.funs = c(mean = "mean", sd = "sd"))

    gg_df <- tidyr::gather(df_plot, key = type,
                           value = value, -tt) %>%
        tidyr::separate(col = type, into = c("var", "measure"),
                 sep = "_") %>%
        tidyr::spread(key = measure, val = value)

    N <- sum(X_df[1, -c(ncol(X_df)-1, ncol(X_df))])
    L <- max(X_df$ll)

    g <- ggplot2::ggplot(data = gg_df, ggplot2::aes(x = tt)) + 
        ggplot2::geom_line(ggplot2::aes(y = mean / N, col = var,
                                              group = var), size = 2) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = (mean -2 * sd)/N,
                                          ymax = (mean + 2 * sd)/N,
                                          fill = var), 
                             alpha = .4) +
        ggplot2::labs(x = "Time", y = "% of Individuals",
                      col = "Mean", fill = "95% CI",
                      title = "Time vs. % of Individuals",
                      subtitle = sprintf("L = %d Simulations", L)) +
        ggplot2::theme_bw() +
        ggplot2::scale_color_manual(values = c("blue", "red"),
                                      labels = c("S", "I"),
                                      name = "Mean") +
        ggplot2::scale_fill_manual(values = c("blue", "red"),
                                   labels = c("S", "I"),
                                   name = "95% CI")  
    g
})


