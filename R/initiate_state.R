#' Providing starting values for state variables
#'
#' @param constart Starting abundance for consumers
#' @param restart Starting concentration for resources
#' @param pars Model parameters from make_par_list()
#'
#' @return
#' @export
#'
#' @examples
#' initiate_state(pars = make_par_list())
initiate_state <- function(constart = 10, restart = 1, vars = pars){
  c(rep(constart, vars$nconsumers), rep(restart, vars$resnum))
}
