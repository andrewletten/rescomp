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
#' This is fine if the result is passed directly to `spec_rescomp()`, but may fail if the result must be combined with other `rescomp_funcresp` first.
#'
#' @returns S3 object of class `rescomp_funcresp`.
#' @export
#'
#' @examples
#'
#' # Type 1 functional response with fixed growth rates
#' spec_funcresp_custom(
#'   function(resources, params) {
#'     growth_rates <- c(0.2, 0.3)
#'     outer(growth_rates, resources)
#'   },
#'   spnum = 2
#' )
spec_funcresp_custom <- function(func, spnum = NULL, resnum = NULL) {
  funcresp <- list(func = func, spnum = spnum, resnum = resnum)
  class(funcresp) <- c("rescomp_funcresp_custom", "rescomp_funcresp")
  return(funcresp)
}
