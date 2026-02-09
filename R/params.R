# TODO: Remove rescomp_params_list, and just use a regular list.

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
#' @param func A function that takes `t` (time) and returns a number to use as a parameter.
#' @param display_values A numeric vector of values of the parameter to use in `plot_funcresp()`.
#'     If not specified here, must be specified in `plot_funcresp()`.
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
rescomp_param_custom <- function(func, display_values = NULL) {
  param <- list(func = func, display_values = display_values)
  class(param) <- c("rescomp_param_custom", "rescomp_param")
  return(param)
}

#' Create a rescomp parameter using a sine/square/triangle wave
#'
#' Produces an object suitable to include in a `rescomp_param_list`.
#' Triangle and square waves are phase-shifted to be similiar in shape to a sine wave with the same period and offset, such that the peaks and troughs occur in the same places.
#'
#' @param period The period of the wave.
#' @param min The minimum value of the parameter; the mean minus the amplitude.
#' @param max The maximum value of the parameter; the mean plus the amplitude.
#' @param offset The phase shift of the wave. For a sine or triangle wave the time at which its value is equal to the mean and increasing. For a square wave, the time at which it increases to the maximum value.
#' @param display_values A numeric vector of values of the parameter to use in `plot_funcresp()`.
#'
#' @returns S3 object of class `rescomp_param`.
#' @export
#'
#' @examples
#' # TODO
rescomp_param_sine <- function(period = 1, min = 0, max = 1, offset = 0, display_values = c(min, max)) {
  param <- list(period = period, mean = (min + max) / 2, amplitude = (max - min) / 2, offset = offset, display_values = display_values)
  class(param) <- c("rescomp_param_sine", "rescomp_param")
  return(param)
}

#' @rdname rescomp_param_sine
#' @export
rescomp_param_triangle <- function(period = 1, min = 0, max = 1, offset = 0, display_values = c(min, max)) {
  param <- list(period = period, min = min, max = max, offset = offset, display_values = display_values)
  class(param) <- c("rescomp_param_triangle", "rescomp_param")
  return(param)
}

#' @rdname rescomp_param_sine
#' @export
rescomp_param_square <- function(period = 1, min = 0, max = 1, offset = 0, display_values = c(min, max)) {
  param <- list(period = period, min = min, max = max, offset = offset, display_values = display_values)
  class(param) <- c("rescomp_param_square", "rescomp_param")
  return(param)
}

#' Get params at an instant in time from a `rescomp_param` object
#'
#' This function is normally only for internal use, but is exported to aid users in debugging their created `rescomp_param` objects.
#'
#' @param param_obj An object of class `rescomp_param`.
#' @param t The time at which to get the parameters.
#'
#' @returns A numeric vector of length 1; the parameter value.
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

#' @export
get_params.rescomp_param_sine <- function(param_obj, t) {
  t_scaled <- (t - param_obj$offset) / param_obj$period
  return(param_obj$mean + param_obj$amplitude * sin(2 * pi * t_scaled))
}

#' @export
get_params.rescomp_param_square <- function(param_obj, t) {
  t_scaled <- (t - param_obj$offset) / param_obj$period
  if (t_scaled %% 1 < 0.5) {
    return(param_obj$max)
  } else {
    return(param_obj$min)
  }
}

#' @export
get_params.rescomp_param_triangle <- function(param_obj, t) {
  t_scaled <- (t - param_obj$offset) / param_obj$period
  t_scaled <- (t_scaled + 0.25) %% 1
  if (t_scaled < 0.5) {
    # Rising segment
    return(param_obj$min + (param_obj$max - param_obj$min) * t_scaled * 2)
  } else {
    # Falling segment
    return(param_obj$max - (param_obj$max - param_obj$min) * (t_scaled - 0.5) * 2)
  }
}

#' Get the display values for `plot_funcresp()` from a `rescomp_param` object
#'
#' `plot_funcresp()` should produce facet plots, such that the functional responses are shown with the parameter fixed at each of its display values.
#'
#' @param param_obj An object of class `rescomp_param`.
#'
#' @returns A numeric vector of values of the parameter to use for plotting.
#' @noRd
#'
#' @examples
#' # TODO
get_display_values <- function(param_obj) {
  UseMethod("get_display_values")
}

#' @export
get_display_values.rescomp_param <- function(param_obj) {
  return(param_obj$display_values)
}
