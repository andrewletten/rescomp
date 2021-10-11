#' Simulate resource competition (a convenience wrapper for
#'     `deSolve::ode()`)
#'
#' @param pars S3 object of class `rescomp` returned by
#'     `rescomp::spec_rescomp()`.
#' @param y Optional vector of initial values for state variables. If provided,
#'     overrides values given in `pars`.
#' @param times Optional list of up to two giving total simulation time and pulsing
#'     sequence where relevant (use `rescomp::time_vals()`). If provided,
#'     overrides values given in parms.
#' @param events NULL (events specified in `spec_rescomp()`)
#' @param ... Other arguments passed to `deSolve::ode()`
#'
#' @return A list of two comprising i) the model dynamics and ii) model
#'     specifications.
#' @export
#'
#' @examples
#' pars <- spec_rescomp()
#' sim_rescomp(pars = pars)
sim_rescomp <- function(pars,
                        times,
                        y,
                        events = NULL,
                        ...) {
  if (missing(times)) {
    if (length(pars$pulsefreq) > 1) {
      times <- time_vals(pars$totaltime, pulse = pars$pulsefreq)
    } else if (pars$pulsefreq != 0) {
      times <- time_vals(pars$totaltime, pulse = pars$pulsefreq)
    } else if (!is.null(pars$introseq)) {
      times <- time_vals(pars$totaltime, introseq = unique(pars$introseq))
    } else {
      times <- time_vals(pars$totaltime)
    }
  } else {
    times <- times
    message("totaltime in pars will be overidden\n")
  }

  if (length(times) == 1) {
    events <- NULL
    if (pars$respulse != 0 | pars$mortpulse != 0) {
      warning(strwrap("respulse or mortpulse parameters nonzero but no pulse
                      sequence provided. ", prefix = " "), immediate. = TRUE)
    }
  } else if (any(pars$pulsefreq != 0) & !is.null(pars$introseq)) {
    stop("Currently not possible to have delayed introductions with resource/mortality pulsing")
  } else if (!is.null(pars$introseq)) {
    events <- list(func = eventfun_starttime, time = times$introseq)
  } else if (any(pars$pulsefreq != 0)) {
    if (pars$respulse == 0 & pars$mortpulse == 0) {
      warning(strwrap("Pulse sequence provided but respulse and mortpulse both
                      set to zero. ", prefix = " "), immediate. = TRUE)
    }
    events <- list(func = eventfun_respulse, time = times$pulseseq)
  }

  if (missing(y)) {
    if (!is.null(pars$introseq)) {
      y <- c(rep(0, pars$nconsumers), pars$rinit)
    } else {
      y <- c(pars$cinit, pars$rinit)
    }
  } else {
    y <- y
    message("cinit in pars will be overidden\n")
  }

  mod <- deSolve::ode(
    func = def_cr_ode,
    y = y,
    parms = pars,
    times = times$totaltime,
    events = events,
    ...
  )

  out <- list(mod, pars[])
  return(out)
}
