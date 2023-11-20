#' Create a vector of coefficients using an arbitrary function
#'
#' Produces an object that may be used in place of a vector in the creation of arguments to `spec_rescomp()`.
#'
#' @param func A function that takes `params` (a list of parameters) and returns a vector.
#' @param length The length of the vector returned by `func`.
#'
#' @returns S3 object of class `rescomp_coefs_vector`.
#' @export
#'
#' @examples
#'
#' # Vector with one time-varying coefficient
#' rescomp_coefs_vector_custom(
#'   function(params) {
#'     c(0.2, 0.3, 0.4, params$fourth_coeff)
#'   },
#'   length = 4
#' )
rescomp_coefs_vector_custom <- function(func, length) {
  coefs <- list(func = func, length = length)
  class(coefs) <- c("rescomp_coefs_vector_custom", "rescomp_coefs_vector")
  return(coefs)
}

#' Create a matrix of coefficients using an arbitrary function
#'
#' Produces an object that may be used in place of a matrix in the creation of arguments to `spec_rescomp()`.
#'
#' @param func A function that takes `params` (a list of parameters) and returns a matrix.
#' @param nrow The number of rows in the matrix returned by `func`.
#' @param ncol The number of columns in the matrix returned by `func`.
#'
#' @returns S3 object of class `rescomp_coefs_matrix`.
#' @export
#'
#' @examples
#'
#' # Matrix with one time-varying coefficient
#' rescomp_coefs_matrix_custom(
#'   function(params) {
#'     matrix(c(0.2, 0.3, 0.4, params$fourth_coeff), nrow = 2, ncol = 2)
#'   },
#'   nrow = 2, ncol = 2
#' )
rescomp_coefs_matrix_custom <- function(func, nrow, ncol) {
  coefs <- list(func = func, nrow = nrow, ncol = ncol)
  class(coefs) <- c("rescomp_coefs_matrix_custom", "rescomp_coefs_matrix")
  return(coefs)
}

#' Get coefficients from a vector/matrix or `rescomp_coefs_vector`/`rescomp_coefs_matrix` object
#'
#' Provides a generic interface to a vector/matrix or a `rescomp_coefs_vector`/`rescomp_coefs_matrix` object.
#' Called on a numeric vector/matrix (as appropriate to the function used), this will just return the vector/matrix.
#' Called on a `rescomp_coefs_vector`/`rescomp_coefs_matrix`, this allows time-dependence via `parameters`.
#'
#' @param coefs_obj A numeric vector/matrix or an object of class `rescomp_coefs_vector`/`rescomp_coefs_matrix`.
#' @param params A list of time-dependent parameters.
#'
#' @returns A vector/matrix of coefficients.
#' @noRd
get_coefs_vector <- function(coefs_obj, params) {
  UseMethod("get_coefs_vector")
}

#' @rdname get_coefs_vector
#' @noRd
get_coefs_matrix <- function(coefs_obj, params) {
  UseMethod("get_coefs_matrix")
}

#' Get length of a vector or `rescomp_coefs_vector` object
#'
#' Provides a generic implementation of length() for a vector or a `rescomp_coefs_vector` object.
#'
#' @param coefs_obj A numeric vector or an object of class `rescomp_coefs_vector`.
#'
#' @returns An integer; the length of the vector.
#' @noRd
get_coefs_length <- function(coefs_obj) {
  UseMethod("get_coefs_length")
}

#' Get number of rows or columns in a matrix or `rescomp_coefs_matrix` object
#'
#' Provides generic implementations of nrow() and ncol() for a matrix or a `rescomp_coefs_matrix` object.
#'
#' @param coefs_obj A numeric matrix or an object of class `rescomp_coefs_matrix`.
#'
#' @returns An integer; the number of rows or columns.
#' @noRd
get_coefs_nrow <- function(coefs_obj) {
  UseMethod("get_coefs_nrow")
}

#' @rdname get_coefs_nrow
#' @noRd
get_coefs_ncol <- function(coefs_obj) {
  UseMethod("get_coefs_ncol")
}

#' @export
get_coefs_vector.numeric <- function(coefs_obj, params) {
  return(coefs_obj)
}

#' @export
get_coefs_vector.integer <- get_coefs_vector.numeric

#' @export
get_coefs_matrix.matrix <- function(coefs_obj, params) {
  return(coefs_obj)
}

#' @export
get_coefs_vector.rescomp_coefs_vector_custom <- function(coefs_obj, params) {
  coefs <- coefs_obj$func(params)
  check_coefs(coefs, coefs_obj$length, "`func` of `rescomp_coefs_vector_custom`")
  return(coefs)
}

#' @export
get_coefs_matrix.rescomp_coefs_matrix_custom <- function(coefs_obj, params) {
  coefs <- coefs_obj$func(params)
  check_coefs(coefs, c(coefs_obj$nrow, coefs_obj$ncol), "`func` of `rescomp_coefs_matrix_custom`")
  return(coefs)
}

#' Verify the class, type, and dimensions of coefficients
#'
#' Checks that a vector/matrix/array returned by a user-provided function is returning an object of the appropriate type, numeric mode, and with the expected dimensions.
#' Throws a user-readable error if necessary.
#'
#' If `length(dims) = 1`, expects a vector.
#' If `length(dims) = 2`, expects a matrix.
#' If `length(dims) > 2`, expects an array.
#'
#' @param obj A vector/matrix/array to check, which is required to be numeric.
#' @param dims The expected dimensions; the length of this is used to determine the appropriate type.
#' @param func_name A character vector of length 1 giving the name of the function whose return value is being tested (for diagnostic printing).
#' @param dims_desc A character vector of length equal to the length of dims describing the dimensions (for diagnostic printing).
#'
#' @returns NULL; this function is called for its side effects (throwing an error if necessary).
#' @noRd
#'
#' @examples
#'
#' check_coefs(3:6, 4, "An example function")
#' try(check_coefs(1:4, c(1, 4), "An example function"))
#' try(check_coefs(letters, 26, "`letter_getter()`"))
#' try(check_coefs(1:6, 5, "`func` of `func_wrapper()`", c("spnum", "resnum")))
#' try(check_coefs(matrix(1:6, nrow = 2, ncol = 3), c(3, 2), "A functional response function", c("spnum", "resnum")))
check_coefs <- function(obj, dims, func_name, dims_desc = NULL) {
  if (length(dims) == 1) {
    check_func <- is.vector
    expected_type <- "vector"
    type_article <- "a"
  } else if (length(dims) == 2) {
    check_func <- is.matrix
    expected_type <- "matrix"
    type_article <- "a"
  } else if (length(dims) > 2) {
    check_func <- is.array
    expected_type <- "array"
    type_article <- "an"
  } else {
    cli::cli_abort(c(
      "`length(dims)` must be greater than 0.",
      "x" = glue::glue("`length(dims)` was {length(dims)}.")
    ))
  }

  if (length(dims) == 1) {
    dims_func <- length
    dims_noun <- "length"
    dims_verb <- "was"
  } else {
    dims_func <- dim
    dims_noun <- "dimensions"
    dims_verb <- "were"
  }

  if (!check_func(obj)) {
    cli::cli_abort(c(
      "{func_name} must return {type_article} {expected_type}.",
      "x" = glue::glue("It returned a {class(obj)[[1]]}.")
    ))
  }
  if (!is.numeric(obj)) {
    cli::cli_abort(c(
      "{func_name} must return a numeric {expected_type}.",
      "x" = glue::glue("It returned a {mode(obj)} {expected_type}.")
    ))
  }
  if (!identical(dims_func(obj), as.integer(dims))) {
    if (is.null(dims_desc)) {
      dims_desc <- ""
    } else {
      dims_desc <- glue::glue(" (i.e. {paste('`', dims_desc, '`', sep = '', collapse = ' by ')})")
    }
    cli::cli_abort(c(
      glue::glue("{func_name} must return a {expected_type} matching expected {dims_noun}."),
      "x" = glue::glue("It returned a {expected_type} with {dims_noun} {toString(dims_func(obj))}."),
      "i" = glue::glue("Expected {dims_noun} {dims_verb} {toString(dims)}{dims_desc}.")
    ))
  }

  return(invisible(NULL))
}
