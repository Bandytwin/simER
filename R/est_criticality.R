#' Estimate the third parameter which will lead to near-criticality
#'
#' @param alpha The proportion of nodes that are inhibitory
#'
#' @param e_eff The effective excitatory weight
#'
#' @param i_eff The effective inhibitory weight
#'
#'

est_criticality <- function(alpha = NULL,
                            e_eff = NULL,
                            i_eff = NULL) {

     # check that two of the arguments are NULL
     if(sum(!is.null(alpha),!is.null(e_eff),!is.null(i_eff)) != 2){
          stop("Only one parameter should be left NULL")
     }

     if (is.null(alpha)) {
          # return alpha
          alpha <- (e_eff - 1) / (e_eff + i_eff)
          return(alpha)
     } else if (is.null(e_eff)) {
          # return e_eff
          e_eff <- (alpha*i_eff + 1) / (1 - alpha)
          return(e_eff)
     } else if (is.null(i_eff)) {
          # return i_eff
          i_eff <- ((e_eff - 1) / alpha) - e_eff
          return(i_eff)
     } else {
          # error
          stop("At least one of the three args must be left NULL")
     }

}
