#' Convert object of class deSolve to a data frame and name columns
#'
#' @param model List output from `sim_rescomp()`. First element is an object of
#' class deSolve. Second element is an S3 object of class `rescomp`.
#'
#' @return data frame
#' @export
#'
#'
#'
#' @examples
#' pars <- spec_rescomp()
#' m1 <- sim_rescomp(pars)
#' frame_and_name(m1)
#'
frame_and_name <- function(model) {
  odeobject <- model[[1]]
  pars <- model[[2]]
  frameddf <- as.data.frame(odeobject)
  names(frameddf)[-1] <- c(paste0("N",
                                  1:pars$nconsumers),
                           paste0("R",
                                  letters[c(1:((ncol(frameddf)) -
                                                      (pars$nconsumers + 1)))]))
  return(frameddf)
}
