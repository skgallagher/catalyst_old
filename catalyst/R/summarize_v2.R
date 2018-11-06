
#' Extract number of individuals in each compartment at each time and simulation
#'
#' @param sims output from simulate_catalyst
#' @return Data frame with the first K columns as the number of individuals in each compartment and the next two columns as tt the time and ll the simulation number
extract_X_df <- function(sims){
    L <- length(sims)
    df <- do.call('rbind', lapply(sims, function(el){
        df <- as.data.frame(el$X)
        df$tt <- 1:nrow(df)
        df$ll <- el$ll
        return(df)
    }))
    return(df)
    


}
