#' Providing starting values for state variables
#'
#' @param pars Model parameters from make_par_list()
#'
#' @return vector
#' @export
#'
#' @examples
#' initiate_state(pars = make_par_list())
initiate_state <- function(pars){
  if(length(pars$cinit) == 1){
    c(rep(pars$cinit, times = pars$nconsumers), pars$resconc)
  } else {
    c(pars$cinit, pars$resconc)
  }
}
