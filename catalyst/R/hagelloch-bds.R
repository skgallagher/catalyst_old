#' Find an upper bd on a sir curve for a subset of an infection
#' 
#' @param Nk - number of objects in group
#' @param data data frame with I and R the infection and recovery times, respectively, for one agent
#' @param bd if "upper" then we are looking for upper sus bd
#' @return list of sir_df which has columns t, did_obs, S, I, and R and inds the indices we would put in the group of size Nk
find_S_bd <- function(Nk, data, bd = "upper"){

    df <- arrange(data, I)
    inds <- integer(Nk)
    max_t <- max(df$R)
    sir_df <- data.frame(t = 1:max_t, did_obs = 0, S = 0, I=0, R=0)
    inds[1] <- 1
    ## These three will always be here for "upper"
    ## Make observations on these days
    sir_df$did_obs[df$I[1]] <- 1
    sir_df$did_obs[df$R[1]] <- 1
    ## But data looks like this in between obs
    sir_df[df$I[1]:(df$R[1] - 1), -c(1:2)] <- rep(c(Nk-1, 1, 0),
                                                  each =  df$R[1] - df$I[1])
    sir_df[df$R[1]:nrow(sir_df), -c(1:2)] <- rep(c(Nk-1, 0, 1),
                                             each =  nrow(sir_df) - df$R[1] + 1)
    if(Nk <= 1){
        non_zero_inds <- which(sir_df$did_obs == 1)
        sir_df <- sir_df[non_zero_inds,]
        return(list(sir_df = sir_df, inds = inds))
    }
    ## Take out observation
    k <- 2
    ## Otherwise, add next in line and check monotonicity of R
    for(ii in 2:Nk){
        for(jj in k:nrow(df)){
            test_df <- add_agent(sir_df, df$I[jj], df$R[jj])
            if(test_is_mono(test_df)){
                sir_df <- test_df
                k <- jj + 1
                inds[ii] <- jj
                break
            }
            if(jj == nrow(df)){
                return(0)
            }

        }

    }
    ## Subset and return
    non_zero_inds <- which(sir_df$did_obs == 1)
    sir_df <- sir_df[non_zero_inds,]
    return(list(sir_df = sir_df, inds = inds))

}

#' Add an agent's recovery/ infection times into an existing group
#'
#' @param df data frame with t 1:max(t), did_obs, and S, I, and  R the total number of agents in each state for the given time
#' @param tI the time in which the agent we are adding becomes infected
#' @param tR the time in which the agent we are adding recovers
#' @return updated df with the agent we added
add_agent <- function(df, tI, tR){
    df$did_obs[tI] <- 1
    df$did_obs[tR] <- 1
    ## But data looks like this in between obs
    df$I[tI:(tR - 1)] <- df$I[tI:(tR - 1)] + 1
    df$R[tR:nrow(df)] <- df$R[tR:nrow(df)] + 1
    if(tI > 1){
        df$S[tI:nrow(df)] <- df$S[tI:nrow(df)]  - 1
    }
    return(df)

}


#' Test if S is non-increasing and R is non-decreasing
#'
#' @param df with columns t, S, I, R
#' @return True if monotone false otherwise
test_is_mono <- function(df){
    N <- length(df)
    S_lag <- df$S[-N]
    S <- df$S[-1]
    S_mono <- all(S_lag - S >= 0)
    ## Now R
    R_lag <- df$R[-N]
    R <- df$R[-1]
    R_mono <- all(R - R_lag  >= 0)
    return(S_mono & R_mono)

}


#' Take a Hagelloch DF  to a cumsum SIR df
#'
#' @param df data frame with I and R which are an agents infection time and recovery time, respectively
#' @return sir data frame with columns t I and R which are now the the TOTAL number of agents in I and R at time t
df_to_SIR <- function(df){
    times <- na.omit(sort(unique(c(df$I, df$R))))
    N <- nrow(df)
    sir <- data.frame(t = times, S = 0, I = 0 , R = 0)
    for(tt in times){
        S <- sum(df$I > tt)
        R <- sum(df$R <= tt)
        I <- N - S - R
        sir[which(times == tt), -1] <- c(S, I, R)
    }
    return(sir)
}


#' Find indices of local maxima in a numeric vector
#' 
#' @param x numeric vector
#' @param partial default is TRUE, meaning we check edges
#' @param decreasing default is FALSE which means we look for maxima
#' @param thresh default is 0, which means we look for any difference
#' @return vector of indices corresponding to local maxima
#' @details Thanks https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
find_peaks <- function(x, partial = TRUE, decreasing = FALSE, thresh = 0){
    if (decreasing){
         if (partial){
             which(diff(c(FALSE,diff(x)>0,TRUE))>0)
         }else {
             which(diff(diff(x)>thresh)> thresh)+1
         }
     }else {
         if (partial){
             which(diff(c(TRUE,diff(x)>= -thresh, FALSE))<0)
         }else {
             which(diff(diff(x)>=0)<0)+1
         }
     }

}


#' Smooth out  infectious curve so there is exactly one maximum
#'
#' @param x vector of observations
#' @return indices which constitute a curve with exactly on maximum (edges don't count)
make_one_peak <- function(x){
    global_max <- max(x)
    max_ind <- which.max(x)
    seq1 <- x[1:max_ind]
    seq2 <- x[max_ind:length(x)]
    inds1 <- get_mono_seq(seq1, decreasing = FALSE, orig_inds = 1:max_ind)
    inds2 <- get_mono_seq(seq2, decreasing = TRUE, orig_inds = max_ind:length(x))
    inds <- unique(c(inds1, inds2))
    vals <- x[inds]
    mat <- matrix(c(inds, vals), ncol = 2)
    return(mat)
    
}
    

get_mono_seq <- function(x, orig_inds,decreasing = FALSE){
    is_mono <- FALSE
    y <- orig_inds
    while(!is_mono){
        valley_inds <- find_peaks(x, partial = FALSE,
                                decreasing = !(decreasing))
        peak_inds <- find_peaks(x, partial = FALSE,
                                decreasing = decreasing)
        if(length(peak_inds) > 0){
            x <- x[-valley_inds]
            y <- y[-valley_inds]
        } else{
            is_mono <- TRUE
        }

    }
    return(y)

}
                          
