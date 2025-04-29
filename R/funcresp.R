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
#' @param a A matrix or `rescomp_coefs_matrix`, with one row per species and one column per resource. The attack rate (if using efficiency) or growth rate (if using quota) of each species on each resource.
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
  funcresp <- list(a = a)
  class(funcresp) <- c("rescomp_funcresp_type1", "rescomp_funcresp")
  return(funcresp)
}

#' Define a Holling type 2 functional response
#'
#' Produces an object suitable to pass as the `funcresp` to `spec_rescomp()`.
#' Creates a Holling type 2 functional response with attack rate `a` and handling time `h`.
#' mu_ij(R_j) = (a_ij * R_j) / (1 + a_ij * h_ij * R_j)
#'
#' @param a A matrix or `rescomp_coefs_matrix`, with one row per species and one column per resource. The maximum attack rate (if using efficiency) or growth rate (if using quota) of each species on each resource, when the resource is rare.
#' @param h A matrix or `rescomp_coefs_matrix`, with one row per species and one column per resource. The handling time of each species for each resource, which reduces attack/growth rate when the resource becomes abundant. With h = 0, this is equivalent to a type 1 function response.
#'
#' @returns S3 object of class `rescomp_funcresp`.
#' @export
#'
#' @examples
#' # TODO
funcresp_type2 <- function(a, h) {
  check_coefs_matrix(a)
  check_coefs_matrix(h)
  funcresp <- list(a = a, h = h)
  class(funcresp) <- c("rescomp_funcresp_type2", "rescomp_funcresp")
  return(funcresp)
}

#' Define a Holling type 3 functional response
#'
#' Produces an object suitable to pass as the `funcresp` to `spec_rescomp()`.
#' Creates a Holling type 3 functional response with attack rate `a`, handling time `h`, and exponent `k`.
#' mu_ij(R_j) = (a_ij * (R_j)^k_ij) / (1 + a_ij * h_ij * (R_j)^k_ij)
#'
#' @param a A matrix or `rescomp_coefs_matrix`, with one row per species and one column per resource. The maximum attack rate (if using efficiency) or growth rate (if using quota) of each species on each resource, when the resource is rare.
#' @param h A matrix or `rescomp_coefs_matrix`, with one row per species and one column per resource. The handling time of each species for each resource, which reduces attack/growth rate when the resource becomes abundant.
#' @param k A matrix or `rescomp_coefs_matrix`, with one row per species and one column per resource. The exponent (a.k.a. Hill coefficient) for each species on each resource. With k = 1, this is equivalent to a type 2 functional response. Each k must be greater than 1 for a true type 3 functional response.
#'
#' @returns S3 object of class `rescomp_funcresp`.
#' @export
#'
#' @examples
#' # TODO
funcresp_type3 <- function(a, h, k) {
  check_coefs_matrix(a)
  check_coefs_matrix(h)
  check_coefs_matrix(k)
  funcresp <- list(a = a, h = h, k = k)
  class(funcresp) <- c("rescomp_funcresp_type3", "rescomp_funcresp")
  return(funcresp)
}

#' Define a Monod functional response
#'
#' Produces an object suitable to pass as the `funcresp` to `spec_rescomp()`.
#' Creates a Monod functional response with maximum growth rate `mumax` and half saturation constant `ks`.
#' mu_ij(R_j) = mumax_ij * R_j / (ks_ij + R_j)
#' This an alternative parameterisation of a type 2 functional response, typically used with quota rather than efficiency.
#'
#' @param mumax A matrix or `rescomp_coefs_matrix`, with one row per species and one column per resource. The maximum growth rate (if using quota) or maximum consumption rate (if using efficiency) of each species on each resource.
#' @param ks A matrix or `rescomp_coefs_matrix`, with one row per species and one column per resource. The half saturation constant of each species for each resource; the resource concentration at which growth/consumption rate is half of mumax.
#'
#' @returns S3 object of class `rescomp_funcresp`.
#' @export
#'
#' @examples
#' # TODO
funcresp_monod <- function(mumax, ks) {
  check_coefs_matrix(mumax)
  check_coefs_matrix(ks)
  funcresp <- list(mumax = mumax, ks = ks)
  class(funcresp) <- c("rescomp_funcresp_monod", "rescomp_funcresp")
  return(funcresp)
}

#' Define a Hill functional response
#'
#' Produces an object suitable to pass as the `funcresp` to `spec_rescomp()`.
#' Creates a Hill functional response with maximum growth rate `mumax`, half saturation constant `ks`, and Hill coefficient n.
#' mu_ij(R_j) = mumax_ij * (R_j)^n_ij / (ks_ij + (R_j)^n_ij)
#' This an alternative parameterisation of a type 3 functional response, typically used with quota rather than efficiency.
#'
#' @param mumax A matrix or `rescomp_coefs_matrix`, with one row per species and one column per resource. The maximum growth rate (if using quota) or maximum consumption rate (if using efficiency) of each species on each resource.
#' @param ks A matrix or `rescomp_coefs_matrix`, with one row per species and one column per resource. The half saturation constant of each species for each resource; the resource concentration at which growth/consumption rate is half of mumax.
#' @param n A matrix or `rescomp_coefs_matrix`, with one row per species and one column per resource. The Hill coefficient for each species on each resource. With n = 1, this is equivalent to a Monod functional response.
#'
#' @returns S3 object of class `rescomp_funcresp`.
#' @export
#'
#' @examples
#' # TODO
funcresp_hill <- function(mumax, ks, n) {
  check_coefs_matrix(mumax)
  check_coefs_matrix(ks)
  check_coefs_matrix(n)
  funcresp <- list(mumax = mumax, ks = ks, n = n)
  class(funcresp) <- c("rescomp_funcresp_hill", "rescomp_funcresp")
  return(funcresp)
}

#' Convert a resource vector into a matrix.
#'
#' Takes a resource vector and replicates it into a matrix with `spnum` identical rows.
#'
#' @param resources A vector of resource concentrations.
#' @param spnum The number of species.
#'
#' @returns A matrix with `spnum` rows and a number of columns equal to `length(resource)`, where each row is a copy of `resources`.
#' @noRd
get_resources_matrix <- function(spnum, resources) {
  # TODO: Check for cache-thrashing when using this in the multiplications in the functional responses.
  return(matrix(resources, nrow = spnum, ncol = length(resources), byrow = TRUE))
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
get_funcresp.rescomp_funcresp_type1 <- function(funcresp_obj, spnum, resources, params) {
  aR <- get_coefs(funcresp_obj$a, params) * get_resources_matrix(spnum, resources)
  return(aR)
}

#' @export
get_funcresp.rescomp_funcresp_type2 <- function(funcresp_obj, spnum, resources, params) {
  aR <- get_coefs(funcresp_obj$a, params) * get_resources_matrix(spnum, resources)
  h <- get_coefs(funcresp_obj$h, params)
  return(aR / (1 + aR * h))
}

#' @export
get_funcresp.rescomp_funcresp_type3 <- function(funcresp_obj, spnum, resources, params) {
  aR <- get_coefs(funcresp_obj$a, params) * get_resources_matrix(spnum, resources)^get_coefs(funcresp_obj$k, params)
  h <- get_coefs(funcresp_obj$h, params)
  return(aR / (1 + aR * h))
}

#' @export
get_funcresp.rescomp_funcresp_monod <- function(funcresp_obj, spnum, resources, params) {
  resources <- get_resources_matrix(spnum, resources)
  mumax <- get_coefs(funcresp_obj$mumax, params)
  ks <- get_coefs(funcresp_obj$ks, params)
  return(mumax * resources / (resources + ks))
}

#' @export
get_funcresp.rescomp_funcresp_hill <- function(funcresp_obj, spnum, resources, params) {
  n <- get_coefs(funcresp_obj$n, params)
  resources <- get_resources_matrix(spnum, resources)^n
  mumax <- get_coefs(funcresp_obj$mumax, params)
  ks <- get_coefs(funcresp_obj$ks, params)^n
  return(mumax * resources / (resources + ks))
}

propagate_crnum.rescomp_funcresp_custom <- function(obj, spnum, resnum) {
  if (is.null(obj$spnum)) {
    obj$spnum <- spnum
  }
  if (is.null(obj$resnum)) {
    obj$resnum <- resnum
  }
  return(obj)
}

propagate_crnum.rescomp_funcresp_type1 <- function(obj, spnum, resnum) {
  obj$a <- propagate_crnum(obj$a, spnum, resnum)
  return(obj)
}

propagate_crnum.rescomp_funcresp_type2 <- function(obj, spnum, resnum) {
  obj$a <- propagate_crnum(obj$a, spnum, resnum)
  obj$h <- propagate_crnum(obj$h, spnum, resnum)
  return(obj)
}

propagate_crnum.rescomp_funcresp_type3 <- function(obj, spnum, resnum) {
  obj$a <- propagate_crnum(obj$a, spnum, resnum)
  obj$h <- propagate_crnum(obj$h, spnum, resnum)
  obj$k <- propagate_crnum(obj$k, spnum, resnum)
  return(obj)
}

propagate_crnum.rescomp_funcresp_monod <- function(obj, spnum, resnum) {
  obj$mumax <- propagate_crnum(obj$mumax, spnum, resnum)
  obj$ks <- propagate_crnum(obj$ks, spnum, resnum)
  return(obj)
}

propagate_crnum.rescomp_funcresp_hill <- function(obj, spnum, resnum) {
  obj$mumax <- propagate_crnum(obj$mumax, spnum, resnum)
  obj$ks <- propagate_crnum(obj$ks, spnum, resnum)
  obj$n <- propagate_crnum(obj$n, spnum, resnum)
  return(obj)
}
