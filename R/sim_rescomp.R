#' Simulate resource competition (a convenience wrapper for
#'     `deSolve::ode`)
#'
#' @param y Initial values for state variables.
#' @param parms Parameter list returned by `rescomp::make_par_list`.
#' @param times List. Total simulation time and pulsing if relevant
#'     (use `rescomp::time_vals`)
#' @param events NULL
#' @param ... Other arguments passed to `deSolve::ode`
#'
#' @return Object of class deSolve
#' @export
#'
#' @examples
#' pars <- make_par_list()
#' happenings <- time_vals(total = 1000)
#' init <- initiate_state(pars)
#' sim_rescomp(y = init,
#'     parms = pars,
#'     times = happenings)
#'
sim_rescomp <-  function(
  parms,
  y = initiate_state(parms),
  times,
  events = NULL,
  ...
  ){
  if(length(times) == 1){
    events = NULL
    if(parms$respulse != 0 | parms$mortpulse != 0){
      warning(strwrap("respulse or mortpulse parameters nonzero but no pulse
                      sequence provided", prefix = " "), immediate. = TRUE)
    }
  } else {
    if(parms$respulse == 0 & parms$mortpulse == 0){
      warning(strwrap("Pulse sequence provided but respulse and mortpulse both
                      set to zero", prefix = " "), immediate. = TRUE)
    }
    events = list(func = eventfun_respulse, time = times$pulseseq)
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
