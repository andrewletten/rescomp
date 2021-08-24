#' Simulate resource competition (a convenience wrapper for
#'     `deSolve::ode`)
#'
#' @param pars Parameter list returned by `rescomp::spec_rescomp`.
#' @param y Vector of initial values for state variables. If provided,
#'     overrides values given in parms.
#' @param times List of up to 2 giving total simulation time and pulsing
#'     sequence where relevant (use `rescomp::time_vals`). If provided,
#'     overrides values given in parms.
#' @param events NULL (events specified in `spec_rescomp`)
#' @param ... Other arguments passed to `deSolve::ode`
#'
#' @return An object of class deSolve
#' @export
#'
#' @examples
#' pars <- spec_rescomp()
#' sim_rescomp(pars = pars)
#'
sim_rescomp <-  function(
  pars,
  times,
  y,
  events = NULL,
  ...
  ){
  if (missing(times)) {
    if (pars$pulsefreq == 0){
      times <- time_vals(pars$totaltime)
    } else {
      times <- time_vals(pars$totaltime, pulse = pars$pulsefreq)
    }
  } else {
    times <- times
    message("totaltime in pars will be overidden\n")
  }

  if(length(times) == 1){
    events = NULL
    if(pars$respulse != 0 | pars$mortpulse != 0){
      warning(strwrap("respulse or mortpulse parameters nonzero but no pulse
                      sequence provided. ", prefix = " "), immediate. = TRUE)
    }
  } else {
    if(pars$respulse == 0 & pars$mortpulse == 0){
      warning(strwrap("Pulse sequence provided but respulse and mortpulse both
                      set to zero. ", prefix = " "), immediate. = TRUE)
    }
    events = list(func = eventfun_respulse, time = times$pulseseq)
  }

  if (missing(y)){
    y <- initiate_state(pars)
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
