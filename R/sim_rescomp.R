#' Simulate resource competition (a convenience wrapper for
#'     `deSolve::ode`)
#'
#' @param parms Parameter list returned by `rescomp::make_par_list`.
#' @param y Vector of initial values for state variables. If provided,
#'     overrides values given in parms.
#' @param times List of up to 2 giving total simulation time and pulsing
#'     sequence where relevant (use `rescomp::time_vals`). If provided,
#'     overrides values given in parms.
#' @param events NULL (events specified in `make_par_list`)
#' @param ... Other arguments passed to `deSolve::ode`
#'
#' @return Matrix of class deSolve
#' @export
#'
#' @examples
#' pars <- make_par_list()
#' sim_rescomp(parms = pars)
#'
sim_rescomp <-  function(
  parms,
  times,
  y,
  events = NULL,
  ...
  ){
  if (missing(times)) {
    if (parms$pulsefreq == 0){
      times <- time_vals(parms$totaltime)
    } else {
      times <- time_vals(parms$totaltime, pulse = parms$pulsefreq)
    }
  } else {
    times <- times
    message("totaltime in parms will be overidden\n")
  }

  if(length(times) == 1){
    events = NULL
    if(parms$respulse != 0 | parms$mortpulse != 0){
      warning(strwrap("respulse or mortpulse parameters nonzero but no pulse
                      sequence provided. ", prefix = " "), immediate. = TRUE)
    }
  } else {
    if(parms$respulse == 0 & parms$mortpulse == 0){
      warning(strwrap("Pulse sequence provided but respulse and mortpulse both
                      set to zero. ", prefix = " "), immediate. = TRUE)
    }
    events = list(func = eventfun_respulse, time = times$pulseseq)
  }

  if (missing(y)){
    y <- initiate_state(parms)
  } else {
    y <- y
    message("cinit in parms will be overidden\n")
  }

  mod <- deSolve::ode(
    func = def_cr_ode,
    y = y,
    parms = parms,
    times = times$totaltime,
    events = events,
    ...
  )
  mod
}
