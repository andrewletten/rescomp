#' Define an event which instantaneously changes species concentrations using an arbitrary function
#'
#' Produces an event object suitable for building event schedules to pass `spec_rescomp()`.
#'
#' @param func A function that takes `species` (a numeric vector of species concentrations) and `params` (a list of parameters) and returns a numeric vector of species concentrations.
#' @param spnum The number of species
#'
#' @details
#'
#' If `spnum` is NULL, `spec_rescomp()` will attempt to infer it.
#' This is fine if the result is used directly in an event schedule, but may fail if the result must be combined with other `rescomp_event`s first.
#'
#' @returns S3 object of class `rescomp_event`.
#' @export
#'
#' @examples
#' # TODO
spec_event_sp_custom <- function(func, spnum = NULL, resnum = NULL) {
  event <- list(func = func, spnum = spnum)
  class(event) <- c("rescomp_event_sp_custom", "rescomp_event")
  return(event)
}

#' Define an event which instantaneously changes resource concentrations using an arbitrary function
#'
#' Produces an event object suitable for building event schedules to pass `spec_rescomp()`.
#'
#' @param func A function that takes `resources` (a numeric vector of resource concentrations) and `params` (a list of parameters) and returns a numeric vector of resource concentrations.
#' @param resnum The number of resources
#'
#' @details
#'
#' If `resnum` is NULL, `spec_rescomp()` will attempt to infer it.
#' This is fine if the result is used directly in an event schedule, but may fail if the result must be combined with other `rescomp_event`s first.
#'
#' @returns S3 object of class `rescomp_event`.
#' @export
#'
#' @examples
#' # TODO
spec_event_res_custom <- function(func, spnum = NULL, resnum = NULL) {
  event <- list(func = func, resnum = resnum)
  class(event) <- c("rescomp_event_res_custom", "rescomp_event")
  return(event)
}

#' Applies a `rescomp_event` object to modify state variables
#'
#' Applies the instantaneous changes specified by a `rescomp_event` object to the full set of state variables (species and resources).
#'
#' This function is normally only for internal use, but is exported to aid users in debugging their created `rescomp_event` objects.
#'
#' @param event_obj An object of class `rescomp_event`.
#' @param species A vector of species concentrations.
#' @param resources A vector of resource concentrations.
#' @param params A list of time-dependent parameters.
#'
#' @returns A vector of state variables, species concentrations followed by resource concentrations.
#' @export
#'
#' @examples
#' # TODO
apply_event <- function(event_obj, species, resources, params) {
  UseMethod("apply_event")
}

#' @export
apply_event.rescomp_event_sp_custom <- function(event_obj, species, resources, params) {
  new_species <- event_obj$func(species, params)
  check_coefs(new_species, length(species), "`func` of `rescomp_event_sp_custom`", "spnum")
  return(c(new_species, resources))
}

#' @export
apply_event.rescomp_event_res_custom <- function(event_obj, species, resources, params) {
  new_resources <- event_obj$func(resources, params)
  check_coefs(new_resources, length(resources), "`func` of `rescomp_event_sp_custom`", "resnum")
  return(c(species, new_resources))
}
