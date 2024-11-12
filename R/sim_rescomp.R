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
  times <- time_vals(pars$totaltime)
  y <- c(pars$cinit, pars$rinit)

  mod <- deSolve::ode(
    func = def_cr_ode,
    y = y,
    parms = pars,
    times = times$totaltime,
    events = list(
      func = ode_event_func,
      time = pars$event_schedule_df$time
    ),
    ...
  )

  out <- list(mod, pars[])
  return(out)
}
