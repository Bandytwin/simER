#' @title Initialize a random, homogenous network
#'
#' @description Initialize a network
#'
#' @param n The number of nodes in the network
#'
#' @param p The probability that node i feeds into node j (and visa-versa)
#'
#' @param k (optional) The expected degree of the network
#'
#' @param alpha The proportion of neurons that are inhibitory
#'
#' @param e_eff The effective excitatory weight
#'
#' @param i_eff The effective inhibitory weight
#'
#' @import data.table stringr

init_network <- function(n,
                         p,
                         alpha = 0.1,
                         e_eff,
                         i_eff,
                         normal_weigths = F) {

     # get the number of inhibitor nodes from alpha
     n_inhib <- round(alpha * n)

     # convert the effective weights to synapse strengths
     w_e <-  e_eff / (n * p)
     w_i <-  i_eff / (n * p)

     # initialize the network connections
     net <- rbinom(n*n, 1, p)
     net <- matrix(net, nrow = n, ncol = n)

     # set inhibitory nodes
     net[, 1:n_inhib] <- -1 * w_i * net[, 1:n_inhib]

     # set excitatory nodes
     net[, (n_inhib+1):n] <- w_e * net[, (n_inhib+1):n]

     # return network
     return(net)

}
