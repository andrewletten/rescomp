#' Define an event to be used by `event_set_introseq`
#'
#' Produces an event object which applies the appropriate changes from an `event_set_introseq`, according to the time.
#'
#' @param times,concentrations The `times` and `concentrations` vectors passed to `event_set_introseq()`.
#'
#' @returns S3 object of class `rescomp_event`.
#' @noRd
#'
#' @examples
#' # TODO
event_introseq <- function(times, concentrations) {
  event <- list(times = times, concentrations = concentrations)
  class(event) <- c("rescomp_event_introseq", "rescomp_event")
  return(event)
}

#' @export
apply_event.rescomp_event_introseq <- function(event_obj, species, resources, params, time) {
  concentrations <- get_coefs(event_obj$concentrations)
  concentrations[event_obj$times != time] <- 0
  species <- species + concentrations
  return(c(species, resources))
}

#' Define a schedule of one-off species introductions: one for each species.
#'
#' Produces an event schedule object suitable for passing in the `events` argument to `spec_rescomp()`.
#'
#' @param times A numeric vector, of length `spnum`, of times at which each species should be introduced.
#' @param concentrations A numeric vector or `rescomp_coefs_vector`, of length `spnum`, of species concentrations; the concentration of each species that should be added at the corresponding time.
#' @param priority A number. If two events occur at the same time, the event with the lower priority number is processed first.
#'
#' @returns S3 object of class `rescomp_event_schedule`.
#' @export
#'
#' @examples
#' # TODO
event_set_introseq <- function(times, concentrations, priority = 0) {
  schedule <- list(
    event_obj = event_introseq(times, concentrations),
    times = unique(sort(times)),
    priority = priority
  )
  class(schedule) <- c("rescomp_event_set_introseq", "rescomp_event_schedule")
  return(schedule)
}

#' @export
get_event_times.rescomp_event_set_introseq <- function(event_schedule_obj, totaltime) {
  return(event_schedule_obj$times)
}
