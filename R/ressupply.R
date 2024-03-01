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
#' ressupply <- spec_ressupply_custom(
#'   function(resources, params) {
#'     conversion <- params$conversion * resources[1]
#'     return(c(params$supply - conversion, conversion))
#'   },
#'   resnum = 2
#' )
#' get_ressupply(ressupply, c(10, 20), list(supply = 3, conversion = 0.2))
spec_ressupply_custom <- function(func, resnum = NULL) {
  ressupply <- list(func = func, resnum = resnum)
  class(ressupply) <- c("rescomp_ressupply_custom", "rescomp_ressupply")
  return(ressupply)
}
