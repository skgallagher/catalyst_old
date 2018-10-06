context("hagelloch data set testing")

test_that("Hagelloch Data format is working", {
    
    library(surveillance)
    data(hagelloch.df)
    ## Add S, I, R columns to hagelloch.df
    hagelloch.df$S <- 0
    hagelloch.df$I <- round(hagelloch.df$tI)
    hagelloch.df$R <- round(hagelloch.df$tR)



    df <- hagelloch.df
    state_names <- c("S", "I", "R")
    df_names <- state_names
    state_levels <- 1:length(state_names)
    T <- max(df[, df_names])
    N <- nrow(df)
    row <-  hagelloch.df[1, ]
    vec_length <- T + 1


    vec <- extract_times(row, df_names, state_levels, vec_length)
    expect_equal(max(which(vec == 1)), 22)
    expect_equal(max(which(vec == 2)), 29)

    ## Make agent data
    devtools::load_all("~/catalyst/catalyst")
    agent_data <- make_agent_data(hagelloch.df)
    expect_equal(dim(agent_data), c(T+1, N))

})

