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

#' Get growth rates from a `rescomp_funcresp` object
#'
#' Gets the growth rates of each species on each resource, given resource concentrations.
#' These must be combined, according to whether the resouces are essential or substitutable, to get the overall growth rate for each species.
#'
#' @param funcresp_obj An object of class `rescomp_funcresp`.
#' @param spnum The number of species.
#' @param resources A vector of resource concentrations.
#' @param params A list of time-dependent parameters.
#'
#' @returns A matrix of species growth rates on each resource, with `spnum` rows and `length(resources)` columns.
#' @noRd
get_funcresp <- function(funcresp_obj, spnum, resources, params) {
  UseMethod("get_funcresp")
}

#' @export
get_funcresp.rescomp_funcresp_custom <- function(funcresp_obj, spnum, resources, params) {
  mat <- funcresp_obj$func(resources, params)

  if (!is.matrix(mat)) {
    cli::cli_abort(c(
      "`func` of `funcresp_custom` must return a matrix.",
      "x" = glue::glue("`func` returned a {class(mat)[[1]]}.")
    ))
  }
  model_dims <- as.integer(c(spnum, length(resources)))
  if (!identical(dim(mat), model_dims)) {
    cli::cli_abort(c(
      glue::glue("`func` of `funcresp_custom` must return a matrix matching dimensions of model."),
      "i" = glue::glue("`func` returned matrix with dimensions {toString(dim(mat))}."),
      "i" = glue::glue("Model has dimensions {toString(model_dims)} (i.e. `spnum` by `resnum`).")
    ))
  }

  return(mat)
}
