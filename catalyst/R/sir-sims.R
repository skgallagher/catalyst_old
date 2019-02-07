## SKG
## Feb. 7, 2019 -- happy Birthday Sean!
## SIR models -- simulating



#' Simulate an SIR model
#'
#' @param params vector of named parameters
#' @param mod_fxn Default is sir_bin.  See details for more
#' @param prob_fxn fiunction to extract probability of S going to I at time t-1 to t.  Default is km_prob which is which is beta * I(t-1) / N
#' @param T total number of time 1:T.  Default is 50.
#' @param L total number of simulations.  Default is 100.
#' @param K total number of compartments.  Default is 3.
#' @param X0 vector of initial values of size K
#' @return list of summary df with average and variance at each time step, and the actual simulations contained in 3 matrices, S_mat, I_mat, and R_mat
simulate_SIR <- function(params, mod_fxn = sir_bin,
                         prob_fxn = km_prob,
                         T = 50, L = 100, K = 3,
                         X0 = c(950, 50, 0)){

    ## Initialize S, I, and R states
    S_mat <- matrix(0, nrow = T, ncol = L)
    S_mat[1, ] <- X0[1]
    I_mat <- matrix(0, nrow = T, ncol = L)
    I_mat[1, ] <- X0[2]
    R_mat <- matrix(0, nrow = T, ncol = L)
    R_mat[1, ] <- X0[3]
    for(ll in 1:L){
        sim <- mod_fxn(params, T, K, X0, prob_fxn)
        S_mat[, ll] <- sim[, 1]
        I_mat[, ll] <- sim[, 2]
        R_mat[, ll] <- sim[, 3]
    }
    ## Extract ave
    S_ave <- rowMeans(S_mat)
    I_ave <- rowMeans(I_mat)
    R_ave <- rowMeans(R_mat)
    df_ave <- data.frame(t = 1:T, S= S_ave, I= I_ave, R= R_ave, type = "ave")
    ## Extract var
    S_var <- rowVar(S_mat)
    I_var <- rowVar(I_mat)
    R_var <- rowVar(R_mat)
    df_var <- data.frame(t = 1:T,  S = S_var, I= I_var, R = R_var, type = "var")
#    sum_df <- plyr::join(df_ave, df_var)

    return(list(df_ave = df_ave,
                df_var = df_var,
                S_mat = S_mat,
                I_mat = I_mat,
                R_mat = R_mat))
}
                         
#' Calculate the row variance of a matrix
#'
#' @param mat a matrix
#' @return a vector of the variance of each row
rowVar <- function(mat){
    out <- apply(mat, 1, var)
    return(out)
}


#' Calculate the stochastic SIR-CM binomial from KM equations
#'
#' @param params c(beta, gamma)
#' @param T total number of time steps
#' @param K total number of states
#' @param X0 vector of initial states of size K
#' @param prob_fxn fiunction to extract probability of S going to I at time t-1 to t.  Default is KM prob which is BI(t-1)/N
#' @return matrix of size T x 3 where first column is S, second is I, and third is R.
sir_bin <- function(params, T, K, X0,
                    prob_fxn = km_prob){
    N <- sum(X0)
    mat <- matrix(0, nrow = T, ncol = 3)
    mat[1, ] <- X0
    for(tt in 2:T){
        pt <- prob_fxn(params, mat[tt-1, 2], N)
        gamma <- params["gamma"]
        mat[tt, 1] <- mat[tt-1,1] - rbinom(1, mat[tt-1,1], pt)
        mat[tt, 3] <- mat[tt-1,3] + rbinom(1, mat[tt-1, 2], gamma)
        mat[tt, 2] <- N - mat[tt, 1] -  mat[tt, 3]
    }
    return(mat)
}

#' Proability for KM SIR S to I
#'
#' @param params named vector "beta"
#' @param I number of infectious
#' @param N total number of individuals
#' @return probability of going from S to I
km_prob <- function(params, I, N){
    if(!any(names(params) %in% "beta")){
        stop("need a beta")
    }
    return(params["beta"] * I / N)
}

#' Proability for Reed-Frost SIR S to I
#'
#' @param params named vector "beta"
#' @param I number of infectious
#' @param N total number of individuals
#' @return probability of going from S to I
#' @details e.g. prob of transition ins 1- (1- rho)^I
rf_prob <- function(params, I, N){
    if(!any(names(params) %in% "rho")){
        stop("need a rho")
    }
    return(1 - (1 - params["rho"])^I)
}



#' Calculate (negative) log-like of the SIR with given probability function
#'
#' @param params vector of params to optimize either (beta, gamma) or (rho, gamma)
#' @param suff_stats matrix of 3xN where rows are initial agent n state, latest time being susceptible, and latest time being infectious
#' @param prob_fxn probability of going from S to I.  Default is km_prob which is beta I /N
#' @param X matrix Tx3 where the columns are number of S, I, and R at time t
#' @return negative log like
loglike_sir_bin <- function(params, suff_stats,
                            prob_fxn = km_prob,
                            X,
                            T){
    N <- ncol(suff_stats)
    agent_loglike <- numeric(N)
    gamma <- params["gamma"]
    names(gamma) <- NULL
    for(nn in 1:N){
        S_loglike <-  0
        I_loglike <- 0
        a0 <- suff_stats[nn, 1]
        s <- suff_stats[nn, 2]
        t <- suff_stats[nn, 3]
        ## Starts in S
        if(a0 == 1){
            if(s > 2){ #there is at least one chance of not being infected
                pt <- sapply(1:(s-1), function(ii){
                    prob_fxn(params, X[ii, 2], N)
                })
                S_loglike <- sum(log(1-pt))
            }
            if(s < T){ # if agent does become infectious
                S_loglike <- S_loglike + log(prob_fxn(params, X[ss, 2], N))
            } else { # agent does not become infectious
                break # so I_loglike = 0
            }
            if(s < t){# if agent has chance of recovering
                I_loglike <-  (t - s - 1) * log(1 - gamma)
                if(t < T){ # if agent does recover
                    I_loglike <- I_loglike + log(gamma)
                }
            }
            agent_loglike[nn] <- S_loglike + I_loglike

        ## Starts in I
        } else if (a0 == 2){
            if(t > 2){ # at least one chance of not becoming recovered
                I_loglike <- (t-1) * log(1 - gamma)
            }
            I_loglike <- I_loglike + log(gamma)

        } else{
            agent_loglike[nn] <- 0
        }

    }



}

    
