#' @title Estimate the activity distribution of a network
#'
#' @description Generate the probability distribution over active states
#' based on the branching function by solving the master equation. A "state"
#' refers to the proportion of
#'
#' @param branch_fun the brancing function (output from est_branching_fun())
#'
#' @param N the number of nodes in the network
#'
#' @return the expected change in activity level at the next time-step
#'
#' @import stringr data.table

est_activity_dist <- function(branch_fun,
                              N) {

     # get sampling rate of s
     delta_s <- branch_fun[, unique(diff(s))][1]

     # add index to branch_fun
     branch_fun[, m := 1:nrow(branch_fun)]

     ## get table of L_nm to cast into matrix

     # get m and n indices
     L_dat <- data.table(expand.grid(seq(1,(1/delta_s-1)), seq(1,(1/delta_s-1))))
     setnames(L_dat, c("n","m"))

     # get branching function values
     L_dat <- merge(L_dat,
                    branch_fun[, c("m","lambda_s")],
                    by = "m")

     # get value in exponential
     L_dat[, L_exp := exp(-(delta_s*N*((m*lambda_s-n)^2))/(2*m*(1-delta_s*m)))]

     # get L_nm
     L_dat[, L_nm := (delta_s*sqrt(N)*L_exp)/sqrt(delta_s*m*(1-delta_s*m))]

     ## create L matrix and find eigenvector of largest eigenvalue
     L_mat <- dcast(L_dat, n ~ m, value.var = "L_nm")
     eigs <- eigen(L_mat[,-1])
     return(abs(eigs$vectors[,1]))

}
