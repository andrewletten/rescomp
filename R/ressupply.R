#' Create a resource supply rate using an arbitrary function
#'
#' Produces an object suitable to pass as the `ressupply` to `spec_rescomp()`.
#'
#' @param func A function that takes `resources` (a numeric vector of resource concentrations) and `params` (a list of parameters) and returns a vector of rates of change of each resource.
#' @param resnum The number of resources; the expected length of the `resources` argument to `func` and the length of the vector returned by `func`.
#'
#' @details
#' If `resnum` is NULL, `spec_rescomp()` will attempt to infer it.
#' This is fine if the result is passed directly to `spec_rescomp()`, but may fail if the result must be combined with other `rescomp_ressupply` first.
#'
#' @returns S3 object of class `rescomp_ressupply`.
#' @export
#'
#' @examples
#' # Two resources, A and B, with constant supply of A, and A spontaneously converting to B
#' ressupply <- ressupply_custom(
#'   function(resources, params) {
#'     conversion <- params$conversion * resources[1]
#'     return(c(params$supply - conversion, conversion))
#'   },
#'   resnum = 2
#' )
#' get_ressupply(ressupply, c(10, 20), list(supply = 3, conversion = 0.2))
ressupply_custom <- function(func, resnum = NULL) {
  ressupply <- list(func = func, resnum = resnum)
  class(ressupply) <- c("rescomp_ressupply_custom", "rescomp_ressupply")
  return(ressupply)
}

#' Create a resource supply rate using a constant rate resource supply
#'
#' Produces an object suitable to pass as the `ressupply` to `spec_rescomp()`.
#'
#' @param rate A vector or `rescomp_coefs_vector`, with one number per resource. The supply rate of each resource.
#'
#' @returns S3 object of class `rescomp_ressupply`.
#' @export
#'
#' @examples
#' ressupply <- ressupply_constant(c(0.2, 0.3))
#' get_ressupply(ressupply, c(2, 10), list())
#' get_ressupply(ressupply, c(5, 20), list()) # The same as above; constant supply doesn't depend on existing concentration.
#'
#' ressupply <- ressupply_constant(rescomp_coefs_lerp(c(0.2, 0.3), c(0.4, 0.6), "extra_supply"))
#' get_ressupply(ressupply, c(2, 10), list(extra_supply = 0.2))
#' get_ressupply(ressupply, c(2, 10), list(extra_supply = 0.8))
ressupply_constant <- function(rate) {
  check_coefs_vector(rate)
  ressupply <- list(rate = rate, resnum = get_coefs_length(rate))
  class(ressupply) <- c("rescomp_ressupply_constant", "rescomp_ressupply")
  return(ressupply)
}

#' Create a resource supply rate using logistic resource growth
#'
#' Produces an object suitable to pass as the `ressupply` to `spec_rescomp()`.
#'
#' @param r A vector or `rescomp_coefs_vector`, with one number per resource. The intrinsic growth rate of each resource.
#' @param k A vector or `rescomp_coefs_vector`, with one number per resource. The carrying capacity of each resource.
#'
#' @returns S3 object of class `rescomp_ressupply`.
#' @export
#'
#' @examples
#' ressupply <- ressupply_logistic(r = rescomp_coefs_lerp(c(0.2, 0.3), c(0, 0), "growth_inhibition"), k = c(10, 20))
#' get_ressupply(ressupply, c(2, 10), list(growth_inhibition = 0))
#' get_ressupply(ressupply, c(20, 0), list(growth_inhibition = 0))
#' get_ressupply(ressupply, c(2, 10), list(growth_inhibition = 0.8))
ressupply_logistic <- function(r, k) {
  check_coefs_vector(r)
  check_coefs_vector(k)
  check_coefs_coordinate(r, k)
  ressupply <- list(r = r, k = k, resnum = get_coefs_length(r))
  class(ressupply) <- c("rescomp_ressupply_logistic", "rescomp_ressupply")
  return(ressupply)
}

#' Create a resource supply rate using chemostat dynamics
#'
#' Produces an object suitable to pass as the `ressupply` to `spec_rescomp()`.
#'
#' @param dilution A numeric vector or `rescomp_coefs_vector`, of length one.
#' @param concentration A vector or `rescomp_coefs_vector`, with one number per resource. The concentration of each resource in the incoming medium.
#'
#' @returns S3 object of class `rescomp_ressupply`.
#' @export
#'
#' @examples
#' ressupply <- ressupply_chemostat(dilution = 0.01, concentration = rescomp_coefs_lerp(c(0, 0, 0), c(2, 3, 4), "ressupply_scaling"))
#' get_ressupply(ressupply, c(2, 4, 10), list(ressupply_scaling = 0))
#' get_ressupply(ressupply, c(3, 3, 3), list(ressupply_scaling = 1))
ressupply_chemostat <- function(dilution, concentration) {
  check_coefs_vector(dilution, length = 1)
  check_coefs_vector(concentration)
  ressupply <- list(dilution = dilution, concentration = concentration, resnum = get_coefs_length(concentration))
  class(ressupply) <- c("rescomp_ressupply_chemostat", "rescomp_ressupply")
  return(ressupply)
}

#' Get resource supply rates from a `rescomp_ressupply` object
#'
#' Gets the resource supply rates of each resource, given the current resource concentrations.
#'
#' This function is normally only for internal use, but is exported to aid users in debugging their created `rescomp_ressupply` objects.
#'
#' @param ressupply_obj An object of class `rescomp_funcresp`.
#' @param resources A vector of resource concentrations.
#' @param params A list of time-dependent parameters.
#'
#' @returns A vector of rates of change of resource concentrations, of the same length as `resources`.
#' @export
#'
#' @examples
#' # Two resources, A and B, with constant supply of A, and A spontaneously converting to B
#' ressupply <- ressupply_custom(
#'   function(resources, params) {
#'     conversion <- params$conversion * resources[1]
#'     return(c(params$supply - conversion, conversion))
#'   },
#'   resnum = 2
#' )
#' get_ressupply(ressupply, c(10, 20), list(supply = 3, conversion = 0.2))
#' try(get_ressupply(ressupply, c(10, 20, 30), list(supply = 3, conversion = 0.2)))
get_ressupply <- function(ressupply_obj, resources, params) {
  UseMethod("get_ressupply")
}

#' @export
get_ressupply.rescomp_ressupply_custom <- function(ressupply_obj, resources, params) {
  vec <- ressupply_obj$func(resources, params)
  check_coefs(vec, length(resources), "`func` of `ressupply_custom`", "resnum")
  return(vec)
}

#' @export
get_ressupply.rescomp_ressupply_constant <- function(ressupply_obj, resources, params) {
  return(get_coefs_vector(ressupply_obj$rate, params))
}

#' @export
get_ressupply.rescomp_ressupply_logistic <- function(ressupply_obj, resources, params) {
  r <- get_coefs_vector(ressupply_obj$r, params)
  k <- get_coefs_vector(ressupply_obj$k, params)
  return(r * resources * (1 - resources / k))
}

#' @export
get_ressupply.rescomp_ressupply_chemostat <- function(ressupply_obj, resources, params) {
  dilution <- get_coefs_vector(ressupply_obj$dilution, params)
  concentration <- get_coefs_vector(ressupply_obj$concentration, params)
  return(dilution * (concentration - resources))
}

propagate_crnum.rescomp_ressupply_custom <- function(obj, spnum, resnum) {
  if (is.null(obj$resnum)) {
    obj$resnum <- resnum
  }
  return(obj)
}

propagate_crnum.ressupply_constant <- function(obj, spnum, resnum) {
  obj$rate <- propagate_crnum(obj$rate, spnum, resnum)
  return(obj)
}

propagate_crnum.ressupply_logistic <- function(obj, spnum, resnum) {
  obj$r <- propagate_crnum(obj$r, spnum, resnum)
  obj$k <- propagate_crnum(obj$k, spnum, resnum)
  return(obj)
}

propagate_crnum.ressupply_chemostat <- function(obj, spnum, resnum) {
  obj$concentration <- propagate_crnum(obj$concentration, spnum, resnum)
  return(obj)
}
