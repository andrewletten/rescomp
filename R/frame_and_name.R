#' Convert object of class deSolve to a data frame and name columns
#'
#' @param odeobject object of class deSolve
#' @param pars Model parameters from make_par_list()
#'
#' @return data frame
#' @export
#'
#'
#'
#' @examples
#' library(deSolve)
#' pars <- make_par_list()
#' happenings <- time_vals()
#' m1 <- ode(
#'     func = def_cr_ode,
#'     y = initiate_state(vars = pars),
#'     parms = pars,
#'     times = happenings$totaltime,
#'     method = "lsoda"
#' )
#' frame_and_name(m1, pars)
#'
frame_and_name <- function(odeobject, pars) {
  frameddf <- as.data.frame(odeobject)
  names(frameddf)[-1] <- c(paste0("N",
                                  1:pars$nconsumers),
                           paste0("R",
                                  letters[c(1:((ncol(frameddf)) -
                                                      (pars$nconsumers + 1)))]))
  return(frameddf)
}
