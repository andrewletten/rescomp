#' Providing starting values for state variables
#'
#' @param pars Model parameters from spec_rescomp()
#'
#' @return vector
#' @export
#'
#' @examples
#' initiate_state(pars = spec_rescomp())
initiate_state <- function(pars){
  if(length(pars$cinit) == 1){
    c(rep(pars$cinit, times = pars$nconsumers), pars$resconc)
  } else {
    c(pars$cinit, pars$resconc)
  }
}
