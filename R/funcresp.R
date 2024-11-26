#' Define a functional response using an arbitrary function
#'
#' Produces an object suitable to pass as the `funcresp` to `spec_rescomp()`.
#'
#' @param func A function that takes `resources` (a numeric vector of resource concentrations) and `params` (a list of parameters) and returns a matrix of growth rates of each species on each resource.
#' @param spnum The number of species; the number of rows in the matrix returned by `func`.
#' @param resnum The number of resources; the expected length of the `resources` argument to `func` and the number of columns in the matrix returned by `func`.
#'
#' @details
#' If `spnum` or `resnum` are NULL, `spec_rescomp()` will attempt to infer them.
#' This is fine if the result is passed directly to `spec_rescomp()`, but may fail if the result must be combined with other `rescomp_funcresp`s first.
#'
#' @returns S3 object of class `rescomp_funcresp`.
#' @export
#'
#' @examples
#' # Type 1 functional response with fixed growth rates
#' funcresp_custom(
#'   function(resources, params) {
#'     growth_rates <- c(0.2, 0.3)
#'     outer(growth_rates, resources)
#'   },
#'   spnum = 2
#' )
funcresp_custom <- function(func, spnum = NULL, resnum = NULL) {
  funcresp <- list(func = func, spnum = spnum, resnum = resnum)
  class(funcresp) <- c("rescomp_funcresp_custom", "rescomp_funcresp")
  return(funcresp)
}

#' Define a linear functional response
#'
#' Produces an object suitable to pass as the `funcresp` to `spec_rescomp()`.
#' Creates a linear or type 1 functional response with attack rate `a`.
#' mu_ij(R_j) = a_ij * R_j
#'
#' @param a A matrix or `rescomp_coefs_matrix`, with one row per species and one column per resource. The attack rate of each species on each resource.
#'
#' @returns S3 object of class `rescomp_funcresp`.
#' @export
#'
#' @examples
#' funcresp1 <- funcresp_type1(
#'   matrix(c(0.2, 0.4, 0.3, 0.2), nrow = 2)
#' )
#' get_funcresp(funcresp1, 2, c(10, 20), list())
#'
#' funcresp2 <- funcresp_type1(
#'   rescomp_coefs_lerp(
#'     matrix(c(0.2, 0.4, 0.3, 0.2), nrow = 2),
#'     matrix(c(0.2, 0.1, 0.3, 0.1), nrow = 2),
#'     "antibiotic_concentration"
#'   )
#' )
#' get_funcresp(funcresp2, 2, c(10, 20), list(antibiotic_concentration = 0.5))
funcresp_type1 <- function(a) {
  check_coefs_matrix(a)
  nrow <- get_coefs_nrow(a)
  ncol <- get_coefs_ncol(a)
  funcresp <- list(func = function(resources, params) {
    return(get_coefs(a, params) * matrix(resources, nrow = nrow, ncol = ncol, byrow = TRUE)) # TODO: Check for cache-thrashing.
  }, spnum = nrow, resnum = ncol)
  class(funcresp) <- c("rescomp_funcresp_type1", "rescomp_funcresp")
  return(funcresp)
}

#' Get growth rates from a `rescomp_funcresp` object
#'
#' Gets the growth rates of each species on each resource, given resource concentrations.
#' These must be combined, according to whether the resouces are essential or substitutable, to get the overall growth rate for each species.
#'
#' This function is normally only for internal use, but is exported to aid users in debugging their created `rescomp_funcresp` objects.
#'
#' @param funcresp_obj An object of class `rescomp_funcresp`.
#' @param spnum The number of species.
#' @param resources A vector of resource concentrations.
#' @param params A list of time-dependent parameters.
#'
#' @returns A matrix of species growth rates on each resource, with `spnum` rows and `length(resources)` columns.
#' @export
#'
#' @examples
#' funcresp <- funcresp_custom(
#'   function(resources, params) {
#'     growth_rates <- params$scale * c(0.2, 0.3)
#'     outer(growth_rates, resources)
#'   },
#'   spnum = 2
#' )
#' get_funcresp(funcresp, 2, c(1, 4, 5, 6), list(scale = 2))
#' get_funcresp(funcresp, 2, 0.7, list(scale = 0.5))
#' try(get_funcresp(funcresp, 3, 0.7, list(scale = 0.5)))
get_funcresp <- function(funcresp_obj, spnum, resources, params) {
  UseMethod("get_funcresp")
}

#' @export
get_funcresp.rescomp_funcresp_custom <- function(funcresp_obj, spnum, resources, params) {
  mat <- funcresp_obj$func(resources, params)
  check_coefs(mat, c(spnum, length(resources)), "`func` of `funcresp_custom`", c("spnum", "resnum"))
  return(mat)
}

#' @export
get_funcresp.rescomp_funcresp <- function(funcresp_obj, spnum, resources, params) {
  mat <- funcresp_obj$func(resources, params)
  return(mat)
}
