#' Convert ode object to dataframe and name columns
#'
#' @param odeobject ode object from
#' @param pars Model parameters from make_par_list()
#'
#' @return data frame
#' @export
#'
# #' @examples
frame_and_name <- function(odeobject, pars) {
  frameddf <- as.data.frame(odeobject)
  names(frameddf)[-1] <- c(paste0("N",
                                  1:pars$nconsumers),
                           paste0("R",
                                  letters[c(1:((ncol(frameddf)) -
                                                      (pars$nconsumers + 1)))]))
  return(frameddf)
}
