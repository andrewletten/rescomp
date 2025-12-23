#' Define an event which instantaneously changes species concentrations using an arbitrary function
#'
#' Produces an event object suitable for building event schedules to pass `spec_rescomp()`.
#'
#' @param func A function that takes `species` (a numeric vector of species concentrations) and `params` (a list of parameters) and returns a numeric vector of species concentrations.
#' @param spnum The number of species.
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
event_sp_custom <- function(func, spnum = NULL) {
  event <- list(func = func, spnum = spnum)
  class(event) <- c("rescomp_event_sp_custom", "rescomp_event")
  return(event)
}

#' Define an event which instantaneously changes resource concentrations using an arbitrary function
#'
#' Produces an event object suitable for building event schedules to pass `spec_rescomp()`.
#'
#' @param func A function that takes `resources` (a numeric vector of resource concentrations) and `params` (a list of parameters) and returns a numeric vector of resource concentrations.
#' @param resnum The number of resources.
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
event_res_custom <- function(func, resnum = NULL) {
  event <- list(func = func, resnum = resnum)
  class(event) <- c("rescomp_event_res_custom", "rescomp_event")
  return(event)
}

#' Define an event in which the species and resources are diluted by addition of new medium
#'
#' Produces an event object representing a batch transfer by dilution with new medium.
#' First, all existing species and resource concentrations are multiplied by `dilution`.
#' Then, the resource concentrations listed in `resources`, multiplied by 1 - `dilution`, are added.
#'
#' @param dilution A numeric vector or `rescomp_coefs_vector` of length 1 (whose value should be
#'     between 0 and 1) representing the proportion of the original medium retained.
#' @param resources A numeric vector or `rescomp_coefs_vector` of resource concentrations.
#'
#' @returns S3 object of class `rescomp_event`.
#' @export
#'
#' @examples
#' # TODO
event_batch_transfer <- function(dilution, resources) {
  event <- list(dilution = dilution, resources = resources)
  class(event) <- c("rescomp_event_batch_transfer", "rescomp_event")
  return(event)
}

#' Define an event in which a constant amount of some consumers is added to (or removed from) the system
#'
#' Produces an event object representing a pulse of added/removed consumers, e.g. a species introduction.
#'
#' @param species A numeric vector or `rescomp_coefs_vector` of species concentrations, by which the current species concentrations are increased. Can be negative, to decrease species concentrations.
#' @param min_zero If this is TRUE, resulting species concentrations are clamped to a minimum of zero. If this is FALSE, negative values of `species` may reduce species concentrations below zero.
#'
#' @returns S3 object of class `rescomp_event`.
#' @export
#'
#' @examples
#' # TODO
event_sp_add <- function(species, min_zero = TRUE) {
  event <- list(species = species, min_zero = min_zero)
  class(event) <- c("rescomp_event_sp_add", "rescomp_event")
  return(event)
}

#' Define an event in which a constant amount of some resources is added to (or removed from) the system
#'
#' Produces an event object representing a pulse of added/removed resources.
#'
#' @param resources A numeric vector or `rescomp_coefs_vector` of resource concentrations, by which the current resource concentrations are increased. Can be negative, to decrease resource concentrations.
#' @param min_zero If this is TRUE, resulting resource concentrations are clamped to a minimum of zero. If this is FALSE, negative values of `resources` may reduce resource concentrations below zero.
#'
#' @returns S3 object of class `rescomp_event`.
#' @export
#'
#' @examples
#' # TODO
event_res_add <- function(resources, min_zero = TRUE) {
  event <- list(resources = resources, min_zero = min_zero)
  class(event) <- c("rescomp_event_res_add", "rescomp_event")
  return(event)
}

#' Define an event in which the consumer populations are multiplied by some factor
#'
#' Produces an event object a pulse of multiplicatively increased/decreased consumer populations, e.g. a density-independent disturbance which kills consumer species.
#'
#' @param species_mult A numeric vector or `rescomp_coefs_vector` by which to multiply the current species concentrations. Should be greater than 1 for an increase, or less than 1 for a decrease. Should not be negative.
#'
#' @returns S3 object of class `rescomp_event`.
#' @export
#'
#' @examples
#' # TODO
event_sp_mult <- function(species_mult) {
  event <- list(species_mult = species_mult)
  class(event) <- c("rescomp_event_sp_mult", "rescomp_event")
  return(event)
}

#' Define an event in which the resource populations are multiplied by some factor
#'
#' Produces an event object a pulse of multiplicatively increased/decreased resource populations, e.g. a density-independent disturbance which kills prey species.
#'
#' @param resources_mult A numeric vector or `rescomp_coefs_vector` by which to multiply the current resource concentrations. Should be greater than 1 for an increase, or less than 1 for a decrease. Should not be negative.
#'
#' @returns S3 object of class `rescomp_event`.
#' @export
#'
#' @examples
#' # TODO
event_res_mult <- function(resources_mult) {
  event <- list(resources_mult = resources_mult)
  class(event) <- c("rescomp_event_res_mult", "rescomp_event")
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
#' @param time The current simulation time.
#'
#' @returns A vector of state variables, species concentrations followed by resource concentrations.
#' @export
#'
#' @examples
#' # TODO
apply_event <- function(event_obj, species, resources, params, time) {
  UseMethod("apply_event")
}

#' @export
apply_event.rescomp_event_sp_custom <- function(event_obj, species, resources, params, time) {
  new_species <- event_obj$func(species, params)
  check_coefs(new_species, length(species), "`func` of `rescomp_event_sp_custom`", "spnum")
  return(c(new_species, resources))
}

#' @export
apply_event.rescomp_event_res_custom <- function(event_obj, species, resources, params, time) {
  new_resources <- event_obj$func(resources, params)
  check_coefs(new_resources, length(resources), "`func` of `rescomp_event_sp_custom`", "resnum")
  return(c(species, new_resources))
}

#' @export
apply_event.rescomp_event_batch_transfer <- function(event_obj, species, resources, params, time) {
  dilution <- get_coefs(event_obj$dilution)
  incoming_resources <- get_coefs(event_obj$resources)
  species <- species * dilution
  resources <- resources * dilution + incoming_resources * (1 - dilution)
  return(c(species, resources))
}

#' @export
apply_event.rescomp_event_sp_add <- function(event_obj, species, resources, params, time) {
  species <- species + get_coefs(event_obj$species)
  if (event_obj$min_zero) {
    species[which(species < 0)] <- 0
  }
  return(c(species, resources))
}

#' @export
apply_event.rescomp_event_res_add <- function(event_obj, species, resources, params, time) {
  resources <- resources + get_coefs(event_obj$resources)
  if (event_obj$min_zero) {
    resources[which(resources < 0)] <- 0
  }
  return(c(species, resources))
}

#' @export
apply_event.rescomp_event_sp_mult <- function(event_obj, species, resources, params, time) {
  species <- species * get_coefs(event_obj$species_mult)
  return(c(species, resources))
}

#' @export
apply_event.rescomp_event_res_mult <- function(event_obj, species, resources, params, time) {
  resources <- resources * get_coefs(event_obj$resources_mult)
  return(c(species, resources))
}

#' @export
propagate_crnum.rescomp_event_sp_custom <- function(obj, spnum, resnum) {
  if (is.null(obj$spnum)) {
    obj$spnum <- spnum
  }
  return(obj)
}

#' @export
propagate_crnum.rescomp_event_res_custom <- function(obj, spnum, resnum) {
  if (is.null(obj$resnum)) {
    obj$resnum <- resnum
  }
  return(obj)
}

#' @export
propagate_crnum.rescomp_event_batch_transfer <- function(obj, spnum, resnum) {
  obj$resources <- propagate_rnum(obj$resources, resnum)
  return(obj)
}

#' @export
propagate_crnum.rescomp_event_sp_add <- function(obj, spnum, resnum) {
  obj$species <- propagate_cnum(obj$species, spnum)
  return(obj)
}

#' @export
propagate_crnum.rescomp_event_res_add <- function(obj, spnum, resnum) {
  obj$resources <- propagate_rnum(obj$resources, resnum)
  return(obj)
}

#' @export
propagate_crnum.rescomp_event_sp_mult <- function(obj, spnum, resnum) {
  obj$species_mult <- propagate_cnum(obj$species, spnum)
  return(obj)
}

#' @export
propagate_crnum.rescomp_event_res_mult <- function(obj, spnum, resnum) {
  obj$resources_mult <- propagate_rnum(obj$resources, resnum)
  return(obj)
}
