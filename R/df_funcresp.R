#' Turn a vector of species indices into a vector of species names for plotting
#'
#' @param indices Integer of species indices.
#'
#' @returns Character vector of species names.
#' @noRd
get_species_names <- function(indices) {
  paste0("N", indices)
}

#' Turn a vector of resource indices into a vector of resource names for plotting
#'
#' @param indices Integer of resource indices.
#'
#' @returns Character vector of resource names.
#' @noRd
get_resource_names <- function(indices) {
  paste0("Resource ", letters[indices])
}

#' Get the display values for all parameters, allowing manually overwriting or providing missing values
#'
#' @inheritParams plot_funcresp
#'
#' @returns A named list of vectors: each vector is the set of display values for the corresponding parameter.
#' @noRd
#'
#' @examples
#' # TODO
process_display_values <- function(pars, display_values, call = rlang::caller_env()) {
  param_display_values <- lapply(pars$params, get_display_values)

  if (!missing(display_values) && length(display_values) > 0) {
    if (length(names(display_values)) == 0 || any(names(display_values) == "")) {
      cli::cli_abort(c(
        "All elements of {.arg display_values} must be named."
      ), call = call)
    } else if (!all(names(display_values) %in% names(pars$params))) {
      misnamed_display_values <- names(display_values)[!(names(display_values) %in% names(pars$params))]
      cli::cli_abort(c(
        "Names of {.arg display_values} must match names of parameters in {.arg pars$params}.",
        "x" = "{.field {misnamed_display_values}} {?does/do} not appear in {.arg pars$params}."
      ), call = call)
    }
    param_display_values[names(display_values)] <- display_values
  }

  if (any(vapply(param_display_values, is.null, TRUE))) {
    null_display_values <- names(param_display_values)[vapply(param_display_values, is.null, TRUE)]
    cli::cli_abort(c(
      "All parameters in {.arg pars$params} must have display values.",
      "x" = "{.field {null_display_values}} {?does/do} not have display values.",
      "i" = "Display values which cannot be automatically inferred may be provided through the {.arg display_values} argument."
    ), call = call)
  } else if (!all(vapply(param_display_values, is.numeric, TRUE))) {
    non_numeric_display_values <- names(param_display_values)[!vapply(param_display_values, is.numeric, TRUE)]
    cli::cli_abort(c(
      "Elements of {.arg display_values} must be numeric vectors.",
      "x" = "{.arg display_values} of {.field {non_numeric_display_values}} are not numeric."
    ), call = call)
  }

  return(param_display_values)
}

#' Generate ggplot friendly data frame for plotting functional responses
#'
#' @inheritParams plot_funcresp
#'
#' @returns A data frame with functional response values for plotting.
#' @noRd
#'
#' @examples
#' pars <- spec_rescomp()
#' df_funcresp(pars)
df_funcresp <- function(pars, maxx = 1, display_values, madj = FALSE, call = rlang::caller_env()) {
  resource_levels <- seq(0, maxx, length.out = 1000)

  param_display_values <- process_display_values(pars, display_values)

  df <- expand.grid(c(list(x = resource_levels), param_display_values))

  ys <- cbind(
    data.frame(sp = rep(seq_len(pars$spnum), times = nrow(df))),
    do.call(rbind, lapply(seq_len(nrow(df)), function(i) {
      get_funcresp(pars$funcresp, pars$spnum, rep(df[i, 1, drop = TRUE], times = pars$resnum), as.list(df[i, -1, drop = FALSE]))
    }))
  ) |>
    tidyr::pivot_longer(-sp, names_to = "res", names_transform = list(res = as.integer), values_to = "y")

  df <- cbind(
    df[rep(seq_len(nrow(df)), each = pars$spnum * pars$resnum), , drop = FALSE],
    ys
  )

  if (madj) {
    mortdf <- df_mort(pars, display_values)
    df <- merge(df, mortdf)
    df$y <- df$y - df$mort
    df$mort <- NULL
  }

  return(df)
}

#' Generate ggplot friendly data frame for plotting mortality values
#'
#' @inheritParams plot_funcresp
#'
#' @returns A data frame with mortality values for plotting.
#' @noRd
#'
#' @examples
#' pars <- spec_rescomp()
#' df_mort(pars)
df_mort <- function(pars, display_values, call = rlang::caller_env()) {
  param_display_values <- process_display_values(pars, display_values)

  if (length(param_display_values) > 0) {
    df <- expand.grid(param_display_values)
  } else {
    df <- data.frame(matrix(ncol = 0, nrow = 1))
  }

  morts <- data.frame(
    sp = rep(seq_len(pars$spnum), times = nrow(df)),
    mort = unlist(lapply(seq_len(nrow(df)), function(i) {
      get_coefs_vector(pars$mort, as.list(df[i, -1, drop = FALSE]))
    }))
  )

  df <- cbind(
    df[rep(seq_len(nrow(df)), each = pars$spnum), , drop = FALSE],
    morts
  )

  return(df)
}
