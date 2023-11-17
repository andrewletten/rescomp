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
