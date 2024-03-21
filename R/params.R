#' Create a list of named rescomp parameters
#'
#' Produces an object suitable to pass as the `params` to `spec_rescomp`.
#'
#' @param ... Named arguments used to create the params list.
#'
#' @details
#' Must be given zero or more named arguments, which are used to build the params list.
#' `rescomp_param` objects are parsed to allow time dependence, while other objects are passed into the list directly.
#'
#' @returns S3 object of class `rescomp_param_list`.
#' @export
#'
#' @examples
#' params <- rescomp_param_list(
#'   r = 0.2,
#'   s = c(3, 4),
#'   conc = rescomp_param_custom(function(t) {
#'     t^2
#'   })
#' )
#' get_params(params, 0.5)
#' get_params(params, 2)
rescomp_param_list <- function(...) {
  param_list <- list(...)
  class(param_list) <- c("rescomp_param_list", "rescomp_param")
  return(param_list)
}

#' Create a rescomp parameter using an arbitrary function
#'
#' Produces an object suitable to include in a `rescomp_param_list`.
#'
#' @param func A function that takes `t` (time) and returns a an object to use a parameter.
#'
#' @returns S3 object of class `rescomp_param`.
#' @export
#'
#' @examples
#' # Repeated pulsing and exponential decay
#' antibiotic_conc <- rescomp_param_custom(function(t) {
#'   0.5^(10 * (t %% 1))
#' })
#' get_params(antibiotic_conc, 0)
#' get_params(antibiotic_conc, 0.5)
#' get_params(antibiotic_conc, 1)
rescomp_param_custom <- function(func) {
  param <- list(func = func)
  class(param) <- c("rescomp_param_custom", "rescomp_param")
  return(param)
}

#' Get params at an instant in time from a `rescomp_param` object
#'
#' This function is normally only for internal use, but is exported to aid users in debugging their created `rescomp_param` objects.
#'
#' @param param_obj An object of class `rescomp_param`.
#' @param t The time at which to get the parameters.
#'
#' @returns The parameters; various objects depending on the type of `param_obj`.
#' @export
#'
#' @examples
#' # Repeated pulsing and exponential decay
#' antibiotic_conc <- rescomp_param_custom(function(t) {
#'   0.5^(10 * (t %% 1))
#' })
#' get_params(antibiotic_conc, 0)
#' get_params(antibiotic_conc, 0.5)
#' get_params(antibiotic_conc, 1)
#'
#' params <- rescomp_param_list(r = 0.2, antibiotic_conc = antibiotic_conc)
#' get_params(params, 0.5)
get_params <- function(param_obj, t) {
  UseMethod("get_params")
}

#' @export
get_params.rescomp_param_list <- function(param_obj, t) {
  param_list <- param_obj
  class(param_list) <- "list"
  for (name in names(param_list)) {
    if ("rescomp_param" %in% class(param_list[[name]])) {
      param_list[[name]] <- get_params(param_list[[name]], t)
    }
  }
  return(param_list)
}

#' @export
get_params.rescomp_param_custom <- function(param_obj, t) {
  return(param_obj$func(t))
}
