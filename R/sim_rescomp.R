#' Simulate resource competition (a convenience wrapper for
#'     `deSolve::ode()`)
#'
#' @param pars S3 object of class `rescomp` returned by
#'     `rescomp::spec_rescomp()`.
#' @param totaltime Numeric vector of length 1: the total simulation time.
#'     If provided, overrides the value in `pars`.
#' @param cinit Numeric vector of length 1 or length `spnum` specifying
#'     initial consumer state values (densities).
#'     If provided, overrides the value in `pars`.
#' @param rinit Numeric vector of length 1 or length `resnum` specifying
#'     initial resource state values (concentrations).
#'     If provided, overrides the value in `pars`.
#' @param ... Other arguments passed to `deSolve::ode()`
#'
#' @return A list of two comprising i) the model dynamics and ii) model
#'     specifications.
#' @export
#'
#' @examples
#' pars <- spec_rescomp()
#' results1 <- sim_rescomp(pars = pars)
#' plot_rescomp(results1)
#'
#' results2 <- sim_rescomp(pars = pars, totaltime = 100, cinit = 1000)
#' plot_rescomp(results2)
sim_rescomp <- function(pars, totaltime, cinit, rinit, ...) {
  # TODO: For parameters that override the values in `pars`, error-check them the same as spec_rescomp().
  # Write helper functions for error-checking that can be called both here and in spec_rescomp().
  if (!missing(totaltime)) {
    pars$totaltime <- totaltime
    cli::cli_alert_info("Overwriting {.arg totaltime} in {.arg pars}.")
  }
  if (!missing(cinit)) {
    pars$cinit <- cinit
    cli::cli_alert_info("Overwriting {.arg cinit} in {.arg pars}.")
  }
  if (!missing(rinit)) {
    pars$rinit <- rinit
    cli::cli_alert_info("Overwriting {.arg rinit} in {.arg pars}.")
  }

  pars$event_schedule_df <- prepare_event_schedule_df(pars$events, pars$totaltime)

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
