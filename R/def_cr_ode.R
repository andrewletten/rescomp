#' Define consumer resource ODE function
#'
#' @param t Time to simulate over
#' @param y Vector of initial states
#' @param pars A list of model parameters
#'
#' @return Model formula to pass to `sim_rescomp()`
#' @export
#'
#' @examples
#' # TODO
def_cr_ode <- function(t, y, pars) {
  N <- y[1:pars$spnum]
  R <- y[-(1:pars$spnum)]
  params <- get_params(pars$params, t)

  mu <- get_funcresp(pars$funcresp, pars$spnum, R, params)
  ressupply <- get_ressupply(pars$ressupply, R, params)
  if (is.null(pars$efficiency)) {
    quota <- get_coefs_matrix(pars$quota, params)
    efficiency <- 1
  } else {
    efficiency <- get_coefs_matrix(pars$efficiency, params)
    quota <- 1 / efficiency
  }
  mort <- get_coefs_vector(pars$mort, params)

  growth_rates <- mu * N * efficiency
  death_rates <- mort * N
  if (pars$essential) {
    total_growth_rates <- apply(growth_rates, 1, min)
    consumption <- colSums(quota * total_growth_rates)
  } else {
    total_growth_rates <- rowSums(growth_rates)
    consumption <- colSums(quota * growth_rates)
  }

  dN <- total_growth_rates - death_rates
  dR <- ressupply - consumption

  return(list(c(dN, dR)))
}

#' The event function for `sim_rescomp()` to pass to `deSolve::ode()`
#'
#' @param t The current time of the simulation.
#' @param y The vector of current estimates of consumers and resources in the simulation.
#' @param pars The `rescomp` object passed to `sim_rescomp()`.
#'
#' @returns `y`, as modified by events at the current time.
#' @noRd
ode_event_func <- function(t, y, pars) {
  params <- get_params(pars$params, t)

  # TODO: Improve the below with a binary search for the correct times.
  for (event_index in pars$event_schedule_df$event_index[pars$event_schedule_df$time == t]) {
    y <- apply_event(pars$events[[event_index]]$event_obj, y[1:pars$spnum], y[-(1:pars$spnum)], params)
  }
  return(y)
}
