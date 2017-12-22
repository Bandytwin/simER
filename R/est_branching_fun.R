#' @title Calculate branch value at activity S
#'
#' @description Estimate the branching function at a particular value of S
#'
#' @param W_E the effective inhibitory weight
#'
#' @param W_I the effective excitatory weight
#'
#' @param k mean degree of network (N*p)
#'
#' @param alpha the proportion of inhibitory neurons
#'
#' @param s the activity level at the current time-step (proportion of active
#' nodes)
#'
#' @return lamb_s the expected change in activity level at the next time-step
#'
#' @import stringr data.table
#'

get_lamb_s <- function(W_E,
                       W_I,
                       k,
                       alpha,
                       s) {

     # initialize variables
     e_in <- 1
     lamb_s <- 0
     e_const <- k *(1-alpha)*s
     i_const <- k*alpha*s
     epsilon <- 0

     # loop until lamb_s changes by less than epsilon
     while(1) {

          # next increment of lamb_s
          nxt_val <- 0

          # inhibitory intputs
          largest_i_in <- floor(e_in*(W_E/W_I))
          i_in_array <- seq(0,largest_i_in,1)
          #i_fact <- factorial(i_in_array)

          # excitatory inputs
          e_in_array <- rep(e_in,length(i_in_array))
          #e_fact <- factorial(e_in_array)

          # sigmoid of inputs
          sigmoid <- ((e_in_array*W_E) - (i_in_array*W_I)) / k
          sigmoid[sigmoid > 1] <- 1

          # sample poisson pdf
          e_poiss <- dpois(e_in_array,e_const)
          i_poiss <- dpois(i_in_array,i_const)

          # get the next value
          nxt_val <- sum((1/s)*sigmoid*e_poiss*i_poiss)

          # update the branching value
          lamb_s <- lamb_s + nxt_val

          # set epsilon once nxt_val passes threshold
          if (nxt_val > 1e-5){
               epsilon <- 1e-5
          }

          # stop if nxt_val is less than epsilon
          if (nxt_val < epsilon){
               return(lamb_s)
          }


          e_in <- e_in + 1


     }

}

#' @title Generate the branching function
#'
#' @description Estimate the branching function for the given network params
#'
#' @param W_E the effective inhibitory weight
#'
#' @param W_I the effective excitatory weight
#'
#' @param k mean degree of network (N*p)
#'
#' @param alpha the proportion of inhibitory neurons
#'
#' @param s_vec the activity levels (proportion of active nodes) to sample
#' for the branching function
#'
#' @return the expected change in activity level at the next time-step
#'
#' @import stringr data.table
#'

est_branching_fun <- function(W_E,
                              W_I,
                              k,
                              alpha,
                              s_vec = seq(0.0005,1,0.0005)) {

     # initialize output
     branching <- list()

     # loop through s_vec and get branching function
     ind <- 1
     for (s in s_vec) {
          next_branch <- data.table::data.table("s" = s)
          next_branch[, lambda_s := get_lamb_s(W_E = W_E,
                                                  W_I = W_I,
                                                  k = k,
                                                  alpha = alpha,
                                                  s = s)]
          branching[[ind]] <- next_branch
          ind <- ind + 1
          message(ind)
     }
     branching <- data.table::rbindlist(branching)

     return(branching)

}
