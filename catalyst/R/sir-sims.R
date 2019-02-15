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
#' @param det_prob logical.  Default is TRUE, which uses second argument in Binomial beta I / N as the expected value
#' @return list of summary df with average and variance at each time step, and the actual simulations contained in 3 matrices, S_mat, I_mat, and R_mat
simulate_SIR <- function(params, mod_fxn = sir_bin,
                         prob_fxn = km_prob,
                         T = 50, L = 100, K = 3,
                         X0 = c(950, 50, 0),
                         det_prob = TRUE){

    ## Initialize S, I, and R states
    S_mat <- matrix(0, nrow = T, ncol = L)
    S_mat[1, ] <- X0[1]
    I_mat <- matrix(0, nrow = T, ncol = L)
    I_mat[1, ] <- X0[2]
    R_mat <- matrix(0, nrow = T, ncol = L)
    R_mat[1, ] <- X0[3]

    X <- NULL
    if(det_prob){ # expected values with non-random prob
        X <- exp_sir(params, T, K, X0,
                 prob_fxn)
    }
    for(ll in 1:L){
        sim <- mod_fxn(params, T, K, X0, prob_fxn, X)
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
#' @param det_prob Use S to I prob as deterministic
#' @return matrix of size T x 3 where first column is S, second is I, and third is R.
sir_bin <- function(params, T, K, X0,
                    prob_fxn = km_prob,
                    X = NULL){
    N <- sum(X0)
    mat <- matrix(0, nrow = T, ncol = 3)
    mat[1, ] <- X0
    if(is.null(X)){
        X <- mat[1,] # random X
    }
    
    for(tt in 2:T){
        pt <- prob_fxn(params, X[tt-1, 2], N)
        gamma <- params["gamma"]
        mat[tt, 1] <- mat[tt-1,1] - rbinom(1, mat[tt-1,1], pt)
        mat[tt, 3] <- mat[tt-1,3] + rbinom(1, mat[tt-1, 2], gamma)
        mat[tt, 2] <- N - mat[tt, 1] -  mat[tt, 3]
    }
    return(mat)
}



#' Calculate the expected value stochastic SIR-CM binomial
#'
#' @param params c(beta, gamma)
#' @param T total number of time steps
#' @param K total number of states
#' @param X0 vector of initial states of size K
#' @param prob_fxn fiunction to extract probability of S going to I at time t-1 to t.  Default is KM prob which is BI(t-1)/N
#' @return matrix of size T x 3 where first column is S, second is I, and third is R.
exp_sir <- function(params, T, K, X0,
                    prob_fxn = km_prob){

    N <- sum(X0)
    mat <- matrix(0, nrow = T, ncol = 3)
    mat[1, ] <- X0
    for(tt in 2:T){
        pt <- prob_fxn(params, mat[tt-1, 2], N)
        gamma <- params["gamma"]
        mat[tt, 1] <- mat[tt-1,1] - mat[tt-1,1] * pt
        mat[tt, 3] <- mat[tt-1,3] + mat[tt-1, 2] * gamma
        mat[tt, 2] <- N - mat[tt, 1] -  mat[tt, 3]
    }
    return(mat)
    

}

#' Calculate the expected variance of stochastic SIR-CM binomial
#'
#' @param params c(beta, gamma)
#' @param T total number of time steps
#' @param K total number of states
#' @param X0 vector of initial states of size K
#' @param X matrix of T x 3 Expected values of S, I, R at each time step
#' @param prob_fxn fiunction to extract probability of S going to I at time t-1 to t.  Default is KM prob which is BI(t-1)/N
#' @return matrix of size T x 4 where first column is V[S], second is V[I], third is V[R], and fourth is Cov[S, R].
var_sir <- function(params, T, K, X0,
                    X,
                    prob_fxn = km_prob){

    N <- sum(X0)
    mat <- matrix(0, nrow = T, ncol = 4) #V[S], V[I], V[R], Cov[S, R]
    mat[1, ] <- 0
    for(tt in 2:T){

        pt <- prob_fxn(params, X[tt-1, 2], N)
        gamma <- params["gamma"]
        ## V[S]
        mat[tt, 1] <- pt * ( 1- pt) * X[tt-1, 1] +
            (1 - pt)^2 * mat[tt-1, 1]
        ## Cov[S, R]
        mat[tt, 4] <- -mat[tt-1, 1] * gamma * (1 - pt) +
            (1 - pt) * (1 - gamma) * mat[tt-1, 4]
        ## V[R]
        mat[tt, 3] <- X[tt-1, 2] * gamma * (1 - gamma) +
            (1 - gamma)^2 * mat[tt-1, 3] +
            gamma^2 * mat[tt-1, 1] - 
            2 * gamma * (1 - gamma) * mat[tt - 1, 4]
        ## V[I]
        mat[tt, 2] <- mat[tt, 1] +  mat[tt, 3] + 2 * mat[tt, 4]
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
        a0 <- suff_stats[1, nn]
        s <- suff_stats[2, nn]
        t <- suff_stats[3, nn]
        ## Starts in S
        if(a0 == 1){
            if(s > 2){ #there is at least one chance of not being infected
                pt <- sapply(1:(s-1), function(ii){
                    prob_fxn(params, X[ii, 2], N)
                })
                S_loglike <- sum(log(1-pt))
            }
            if(s < T){ # if agent does become infectious
                S_loglike <- S_loglike + log(prob_fxn(params, X[s, 2], N))
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
    return(-sum(agent_loglike))



}


#' Get the loglike for a singular agent
#'
#' @param params vector of params to optimize either (beta, gamma) or (rho, gamma)
#' @param X matrix Tx3 where the columns are number of S, I, and R at time t
#' @param N total number of agents
#' @param a0 initial sate of agent
#' @param s last time agent was susceptible
#' @param t last time agent was infectious
#' @param T total time
#' @param prob_fxn probability of going from S to I.  Default is km_prob which is beta I /N
#' @return total log like for a single agent
agent_loglike_sir <- function(params, X, N,
                              a0, s, t, T,
                              prob_fxn = km_prob){
    gamma <- params["gamma"]
    S_loglike <- 0
    I_loglike <- 0
    if(a0 == 1){
        if(s > 2){ #there is at least one chance of not being infected
            pt <- sapply(1:(s-1), function(ii){
                prob_fxn(params, X[ii, 2], N)
            })
            S_loglike <- sum(log(1-pt))
        }
        if(s < T){ # if agent does become infectious
            S_loglike <- S_loglike + log(prob_fxn(params, X[s, 2], N))
        } else { # agent does not become infectious
           return(S_loglike)
        }
        if(s < t){# if agent has chance of recovering
            I_loglike <-  (t - s - 1) * log(1 - gamma)
            if(t < T){ # if agent does recover
                I_loglike <- I_loglike + log(gamma)
            }
        }
        agent_loglike <- S_loglike + I_loglike
        return(agent_loglike)
        ## Starts in I
    } else if (a0 == 2){
        if(t > 2){ # at least one chance of not becoming recovered
            I_loglike <- (t-1) * log(1 - gamma)
        }
        I_loglike <- I_loglike + log(gamma)
        return(I_loglike)

    } else{
        agent_loglike <- 0
        return(agent_loglike)
    }


}

#' Return negative of agent_loglike_sir
#'
#' @param params vector of params to optimize either (beta, gamma) or (rho, gamma)
#' @param X matrix Tx3 where the columns are number of S, I, and R at time t
#' @param N total number of agents
#' @param a0 initial sate of agent
#' @param s last time agent was susceptible
#' @param t last time agent was infectious
#' @param T total time
#' @param prob_fxn probability of going from S to I.  Default is km_prob which is beta I /N
#' @return negative loglike
neg_agent_loglike_sir <- function(params, X, N, a0, s, t,
                                  prob_fxn = km_prob){

    -1 * agent_loglike_sir(params, X, N, a0, s, t)
}

#' Get loglike for SIR for beta
#'
#' @param X matrix Tx3 where the columns are number of S, I, and R at time t
#' @param N total number of agents
#' @param T total number of time steps
#' @param s scalar last time agent was susceptible
#' @param prob_fxn probability of transitioning.  Default is km_prob which KM beta * I / N
#' @return loglike of beta
loglike_beta_SIR <- function(beta, X, N, T,
                             s, prob_fxn = km_prob){
    params <- c("beta" = beta)
    S_loglike <- 0
    if(s > 2){
        pt <- sapply(1:(s-1), function(ii){
            prob_fxn(params, X[ii, 2], N)
        })
        S_loglike <- sum(log(1-pt))
    }
    if(s < T){ # if agent does become infectious
        S_loglike <- S_loglike + log(prob_fxn(params, X[s, 2], N))
    }
    return(S_loglike)
}




#' Simulate the AM SIR
#'
#' @param params vector of named parameters
#' @param L number of simulations
#' @param A T x N matrix of agent states, A[1,] is filled in
#' @param T total amount of time steps
#' @param N total number of agents
#' @param K total number of states
#' @param agent_update_fxn function to update agents
#' @param permute_inds logical.  Default is TRUE.
#' @return filled in matrix of agent states
am_sir_sims <- function(L = 1, params, A,
                        T, N, K,
                        agent_update_fxn = update_agent_sir,
                        prob_fxn = km_prob,
                        permute_inds = TRUE){
    arr <- array(0, dim = c(L, T, N))
    S_mat <- matrix(0, nrow = T, ncol = L)
    I_mat <- matrix(0, nrow = T, ncol = L)
    R_mat <- matrix(0, nrow = T, ncol = L)


    for(ll in 1:L){
        inds <- 1:N # original indices
        for(tt in 2:(T)){
            state_inds_list <- get_state_inds(A[tt-1, ], K)
            for(kk in 1:K){
                state_inds_k <- state_inds_list[[kk]]
                if(!any(is.na(state_inds_k))){

                    new_states <- agent_update_fxn(params,
                                                   kk, N, state_inds_list,
                                                   prob_fxn, permute_inds)
                    A[tt, state_inds_k] <- new_states
                }
            }
            if(!permute_inds){
                perm_list <- reorder_A(A, tt, inds)
                A <- perm_list$A
                inds <- perm_list$inds
            }
            
        }
        
        summary_list <- summarize_agent_data(A[,order(inds)], K)
        arr[ll, , ] <- summary_list$agent_data
        S_mat[, ll] <- summary_list$X[, 1]
        I_mat[, ll] <- summary_list$X[, 2]
        R_mat[, ll] <- summary_list$X[,3]

    }
    out_list <- list(agent_arr = arr, S_mat = S_mat, I_mat = I_mat,
                     R_mat = R_mat)

    return(out_list)

}
                            

#' Get the state indices for all states
#'
#' @param At vector of length N with current states of each agent
#' @param K total number of states 1:K
#' @return list of length K with indices of who is in each
get_state_inds <- function(At, K){
    state_list <- vector(mode = "list", length = K)
    for(kk in 1:K){
        inds <- which(At == kk)
        if(length(inds) < 1) inds <- NA
        state_list[[kk]] <- inds
    }

    return(state_list)


}


#' Update the agents in state k for the SIR
#'
#' @param params named vector of parameters with "beta" or "rho" and "gamma"
#' @param kk current state
#' @param N total number of agents
#' @param state_inds_list list of length K with indices of who is in each
#' @param prob_fxn default is km_prob
#' @param permute_inds logical.  Default is TRUE
#' @return new_states vector of length of state_inds_list[[kk]]
update_agent_sir <- function(params,
                             kk, N, state_inds_list,
                             prob_fxn = km_prob, permute_inds = TRUE){
    Nk <- length(state_inds_list[[kk]])
    I <- length(state_inds_list[[2]]) #number of I
    new_states <- NULL
    if(kk == 1){ # susceptible
        p <- km_prob(params, I, N)
        new_states <- 1 + rbinom(n = Nk, size = 1, prob = p)
    } else if(kk == 2){
        new_states <- 2 + rbinom(n = Nk, size = 1, prob = params["gamma"])
    } else if (kk == 3){
        new_states <- rep(3, Nk)
    }
    if(!permute_inds){
        new_states <- sort(new_states, decreasing = TRUE)
    }
    return(new_states)

}


#' Reorder rows of A
#' 
#' @param A matrix of size T x N
#' @param tt current time step
#' @param inds vector of indices corresponding to current version of permutation of A
#' @return list of reordered A and permutation with respect to original ordering
reorder_A <- function(A, tt, inds){
    new_inds <- order(A[tt,], decreasing = TRUE)
    A <- A[, new_inds]
    perm_inds <- inds[new_inds]

    return(list(A = A, inds = perm_inds))


}

#' Put output of am_sir_sims into ggplot df
#'
#' @param agent_sims output from am_sir_sims()
#' @return data frame with columns t, model (AM), type (ave or var), variable (S, I, R)
agent_sims_sir_gg <- function(agent_sims){


    S <- agent_sims$S
    I <- agent_sims$I
    R <- agent_sims$R
    T <- nrow(S)
    ##
    S_ave <- rowMeans(S)
    I_ave <- rowMeans(I)
    R_ave <- rowMeans(R)
    df_ave <- data.frame(t = 1:T, S = S_ave, I = I_ave, R = R_ave,
                         model = "AM", type = "ave")
    gg_df <- melt(df_ave, id.vars = c("t", "model", "type"))
    ## var
    ##
    S_var <- rowVar(S)
    I_var <- rowVar(I)
    R_var <- rowVar(R)
    df_var <- data.frame(t = 1:T, S = S_var, I = I_var, R = R_var,
                         model = "AM", type = "var")
    var_m <- melt(df_ave, id.vars = c("t", "model", "type"))
    ## Add var
    gg_df$lower <- gg_df$value - 2 * sqrt(var_m$value)
    gg_df$upper <- gg_df$value + 2 * sqrt(var_m$value)
   
    return(gg_df)
}


#' @param A TxN (or L x N)
time_to_inf <- function(A){
    a0 <- A[1,]
    T <- nrow(A)
    s <- apply(A, 2, function(row){
        out <- which(row == 2)[1]
        if(is.na(out)){
            return(T + 1)
        } else {
            return(out)
        }
    })
    return(s)
    

}
