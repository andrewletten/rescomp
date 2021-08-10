#' Providing starting values for state variables
#'
#' @param constart Starting abundance for consumers
#' @param restart Starting concentration for resources
#' @param vars Model parameters from make_par_list()
#'
#' @return vector
#' @export
#'
#' @examples
#' initiate_state(vars = make_par_list())
initiate_state <- function(vars, constart = 10, restart = vars$resconc){
  c(rep(constart, vars$nconsumers), restart)
}
