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
  quota <- get_coefs_matrix(pars$quota, params)
  mort <- get_coefs_vector(pars$mort, params)

  growth_rates <- mu * N
  death_rates <- mort * N
  if (pars$essential) {
    total_growth_rates <- apply(growth_rates, 1, min)
    consumption <- colSums(pars$quota * total_growth_rates)
  } else {
    total_growth_rates <- rowSums(growth_rates)
    consumption <- colSums(pars$quota * growth_rates)
  }

  dN <- total_growth_rates - death_rates
  dR <- ressupply - consumption

  return(list(c(dN, dR)))
}

#' Timings of happenings
#'
#' @param total Total time.
#' @param step Step size.
#' @param doround Round time units (handles issues with numerical differences
#'     that produce warning messages when pulsing resources and/or consumers).
#' @param pulse Pulsing interval.
#' @param introseq Sequence as vector for consumer introductions.
#'     Vector length must equal spnum.
#'
# #' @return
#' @export
#'
#' @examples
#' time_vals(1000, pulse = 100)
time_vals <- function(total = 1000,
                      step = 0.1,
                      doround = TRUE,
                      pulse,
                      introseq = NULL) {
  time_vals <- list()
  ifelse(doround,
    time_vals$totaltime <- round(seq(0, total, by = step), 1),
    time_vals$totaltime <- seq(0, total, by = step)
  )

  if (missing(pulse)) {
    time_vals$pulseseq <- NULL
  } else if (length(pulse) == 1) {
    time_vals$pulseseq <- round(seq(pulse, total, by = pulse), 1)
  } else {
    time_vals$pulseseq <- round(pulse, 1)
  }

  time_vals$introseq <- introseq

  return(time_vals)
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
