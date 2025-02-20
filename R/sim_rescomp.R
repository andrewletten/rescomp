#' Simulate resource competition (a convenience wrapper for
#'     `deSolve::ode()`)
#'
#' @param pars S3 object of class `rescomp` returned by
#'     `rescomp::spec_rescomp()`.
#' @param ... Other arguments passed to `deSolve::ode()`
#'
#' @return A list of two comprising i) the model dynamics and ii) model
#'     specifications.
#' @export
#'
#' @examples
#' pars <- spec_rescomp()
#' sim_rescomp(pars = pars)
sim_rescomp <- function(pars, ...) {
  times <- seq(0, pars$totaltime, by = 0.1) # TODO: Make step size customisable.
  y <- c(pars$cinit, pars$rinit)

  if (nrow(pars$event_schedule_df) > 0) {
    events <- list(
      func = ode_event_func,
      time = pars$event_schedule_df$time
    )
  } else {
    events <- list()
  }

  mod <- deSolve::ode(
    func = def_cr_ode,
    y = y,
    parms = pars,
    times = times,
    events = events,
    ...
  )

  out <- list(mod, pars[])
  return(out)
}
