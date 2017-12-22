#' Simulate the dynamics of a network instance with no input and random
#' initial conditions.
#'
#' @param net a network (the output from init_network()) to simulate
#'
#' @param n_steps the number of time steps to simulate
#'
#' @param s_1 the proporation of active nodes at time step 1
#'
#' @import data.table stringr

sim_network <- function(net,
                        t_steps = 1e5,
                        s_1 = 0.5,
                        disp = T) {

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

          # if no nodes are on, then end loop and return activity vector
          if (s_t[t_ind+1] == 0){
               return(s_t)
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
