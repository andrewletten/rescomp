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

#' Get coefficients from a vector or `rescomp_coefs_vector` object
#'
#' Provides a generic interface to a vector or a `rescomp_coefs_vector` object.
#' Called on a numeric vector, this will just return the vector.
#' Called on a `rescomp_coefs_vector`, this allows time-dependence via `parameters`.
#'
#' @param coefs_obj A numeric vector or an object of class `rescomp_coefs_vector`.
#' @param params A list of time-dependent parameters.
#'
#' @returns A vector of coefficients.
#' @noRd
get_coefs_vector <- function(coefs_obj, params) {
  UseMethod("get_coefs_vector")
}

#' Get coefficients from a matrix or `rescomp_coefs_matrix` object
#'
#' Provides a generic interface to a matrix or a `rescomp_coefs_matrix` object.
#' Called on a numeric matrix, this will just return the matrix.
#' Called on a `rescomp_coefs_matrix`, this allows time-dependence via `parameters`.
#'
#' @param coefs_obj A numeric matrix or an object of class `rescomp_coefs_matrix`.
#' @param params A list of time-dependent parameters.
#'
#' @returns A matrix of coefficients.
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
