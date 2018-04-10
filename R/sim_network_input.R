#' Simulate the dynamics of a network instance without input in order to get
#' a reasonable steady state
#'
#' @param net a network (the output from init_network()) to simulate
#'
#' @param n_steps the number of time steps to simulate
#'
#' @param s_1 the proporation of active nodes at time step 1
#'
#' @import data.table stringr

init_network_ss <- function(net,
                            t_steps = 500,
                            s_1 = 0.5,
                            disp = F) {

     # get number of nodes in netowrk
     n_nodes <- nrow(net)

     # initialize active node vector
     active_nodes <- rep(0, n_nodes)
     active_nodes[sample(1:n_nodes, n_nodes*s_1)] <- 1

     # initialize vector to hold s_t
     s_t <- s_1

     # loop through and simulate dynamics
     for (t_ind in 1:t_steps) {

          if ((disp) & (t_ind%%100 == 0)) {

               message(t_ind)

          }

          # get vector of input-sums to all nodes
          in_sums <- net %*% active_nodes

          # pass the input-sums through the sigmoid
          sig_in_sums <- lin_sig(in_sums)

          # get new activity vector based on sigmoid values
          active_nodes <- rbinom(n = n_nodes,
                                 p = sig_in_sums,
                                 size = 1)

          # store the proportion of active nodes
          s_t <- c(s_t, sum(active_nodes)/n_nodes)

          # if no nodes are on, then throw error
          if (s_t[t_ind+1] == 0){
               stop("network activity died before t_steps was reached")
          }

     }

     # return the network state
     net_ss <- list("net" = net, "active_nodes" = active_nodes)
     return(net_ss)

}

#' Simulate the dynamics of a steady state network instance with input
#'
#' @param net_ss a network and activity vector that represent steady state
#' (the output from init_network())
#'
#' @param in_symbol a vector of length N of values in [-1,1] that represents
#' added input to the specified node. A value of 0 means there is no extra input
#'
#' @param n_steps the number of time steps to simulate
#'
#' @import data.table stringr

sim_network_input <- function(net_ss,
                              in_symbol,
                              t_steps = 1,
                              disp = F) {

     # extract data from net_ss
     net <- net_ss$net
     active_nodes <- net_ss$active_nodes

     # get number of nodes in netowrk
     n_nodes <- nrow(net)

     # initialize vector to hold activity
     s_t <- sum(active_nodes/n_nodes)

     # loop through and simulate dynamics
     for (t_ind in 1:t_steps) {

          if ((disp) & (t_ind%%100 == 0)) {

               message(t_ind)

          }

          # get vector of input-sums to all nodes (including input)
          in_sums <- (net %*% active_nodes) + in_symbol

          # pass the input-sums through the sigmoid
          sig_in_sums <- lin_sig(in_sums)

          # get new activity vector based on sigmoid values
          active_nodes <- rbinom(n = n_nodes,
                                 p = sig_in_sums,
                                 size = 1)

          # store the proportion of active nodes
          s_t <- c(s_t, sum(active_nodes)/n_nodes)

          # if no nodes are on, then throw warning
          if (s_t[t_ind+1] == 0){
               warning("network activity died before t_steps was reached")
          }

     }

     # return the vector of network activity
     return(s_t)

}

lin_sig <- function(val_in) {

     # pass the input through a linear sigmoid
     val_in[val_in > 1] <- 1
     val_in[val_in < 0] <- 0
     return(val_in)

}
