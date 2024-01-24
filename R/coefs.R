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
  coefs <- list(func = func, dim = length)
  class(coefs) <- c("rescomp_coefs_vector_custom", "rescomp_coefs_custom", "rescomp_coefs_vector", "rescomp_coefs")
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
  coefs <- list(func = func, dim = c(nrow, ncol))
  class(coefs) <- c("rescomp_coefs_matrix_custom", "rescomp_coefs_custom", "rescomp_coefs_matrix", "rescomp_coefs")
  return(coefs)
}

#' Create a set of coefficients by linear interpolation
#'
#' Produces an object that may be used in place of a vector or matrix in the creation of arguments to `spec_rescomp()`.
#' Linearly interpolates between two sets of coefficients according to the value of a time-dependent parameter.
#'
#' Coefficients are taken to have the values in `coef0` at `param = param0`, and the values in `coef1` at `param = param1`, and are linearly interpolated between.
#' Each parameter in the vector/matrix is interpolated independently.
#'
#' @param coefs0,coefs1 Vectors, matrices, or objects of class `rescomp_coefs_vector` or `rescomp_coefs_matrix` to be interpolated between. Either both must be vectors, or both must be matrices.
#' @param param_name A character vector of length 1; the name of the parameter to be interpolated according to.
#' @param param0,param1 The values of the parameter to take as the fixed points for interpolation.
#'
#' @returns S3 object of class `rescomp_coefs_vector` or `rescomp_coefs_matrix`, according to the types of `coefs0` and `coefs1`.
#' @export
#'
#' @examples
#'
#' coefs_vec <- rescomp_coefs_lerp(
#'   c(0, 0, 0),
#'   c(2, 3, 5),
#'   "scale"
#' )
#' get_coefs(coefs_vec, list(scale = 0))
#' get_coefs(coefs_vec, list(scale = 0.5))
#' get_coefs(coefs_vec, list(scale = 1))
#' get_coefs(coefs_vec, list(scale = 1.5))
#'
#' coefs_mat1 <- rescomp_coefs_lerp(
#'   matrix(c(0.2, 0.4, 0.3, 0.2), nrow = 2),
#'   matrix(c(0.2, 0.1, 0.3, 0.1), nrow = 2),
#'   "antibiotic1_concentration",
#'   param0 = 0,
#'   param1 = 200
#' )
#' get_coefs(coefs_mat1, list(antibiotic1_concentration = 0))
#' get_coefs(coefs_mat1, list(antibiotic1_concentration = 100))
#' get_coefs(coefs_mat1, list(antibiotic1_concentration = 200))
#'
#' coefs_mat2 <- rescomp_coefs_lerp(
#'   coefs_mat1,
#'   matrix(c(0.0, 0.0, 0.0, 0.0), nrow = 2),
#'   "antibiotic2_concentration",
#'   param0 = 0,
#'   param1 = 100
#' )
#' get_coefs(coefs_mat2, list(antibiotic1_concentration = 100, antibiotic2_concentration = 50))
rescomp_coefs_lerp <- function(coefs0, coefs1, param_name, param0 = 0, param1 = 1) {
  check_coefs_coordinate(coefs0, coefs1)
  coefs <- list(
    func = function(params) {
      param <- params[[param_name]] # TODO: Verify param exists.
      return((get_coefs(coefs0, params) * (param1 - param) + get_coefs(coefs1, params) * (param - param0)) / (param1 - param0))
    },
    dim = get_coefs_dim(coefs0)
  )
  if (is_coefs_vector(coefs0)) {
    class(coefs) <- c("rescomp_coefs_lerp_vector", "rescomp_coefs_lerp", "rescomp_coefs_vector", "rescomp_coefs")
  } else {
    class(coefs) <- c("rescomp_coefs_lerp_matrix", "rescomp_coefs_lerp", "rescomp_coefs_matrix", "rescomp_coefs")
  }
  return(coefs)
}

#' Get coefficients from a vector/matrix or `rescomp_coefs_vector`/`rescomp_coefs_matrix` object
#'
#' Provides a generic interface to a vector/matrix or a `rescomp_coefs_vector`/`rescomp_coefs_matrix` object.
#' Called on a numeric vector/matrix (as appropriate to the function used), this will just return the vector/matrix.
#' Called on a `rescomp_coefs_vector`/`rescomp_coefs_matrix`, this allows time-dependence via `parameters`.
#' `get_coefs()` works on vector- or matrix-type objects, while the others work only on their specific types.
#'
#' This function is normally only for internal use, but is exported to aid users in debugging their created `rescomp_coefs` objects.
#'
#' @param coefs_obj A numeric vector/matrix or an object of class `rescomp_coefs_vector`/`rescomp_coefs_matrix`.
#' @param params A list of time-dependent parameters.
#'
#' @returns A vector/matrix of coefficients.
#' @export
#'
#' @examples
#' get_coefs(c(0.2, 0.3, 0.4), list())
#' get_coefs(matrix(c(0.2, 0.3, 0.4, 0.2), nrow = 2), list())
#' get_coefs_vector(c(0.2, 0.3, 0.4), list())
#' try(get_coefs_matrix(c(0.2, 0.3, 0.4), list()))
#'
#' coefs <- rescomp_coefs_matrix_custom(
#'   function(params) {
#'     matrix(c(0.2, 0.3, 0.4, params$fourth_coeff), nrow = 2, ncol = 2)
#'   },
#'   nrow = 2, ncol = 2
#' )
#' get_coefs(coefs, list(fourth_coeff = 0.5))
get_coefs <- function(coefs_obj, params) {
  UseMethod("get_coefs")
}

#' @rdname get_coefs
#' @export
get_coefs_vector <- function(coefs_obj, params) {
  check_coefs_vector(coefs_obj)
  UseMethod("get_coefs")
}

#' @rdname get_coefs
#' @export
get_coefs_matrix <- function(coefs_obj, params) {
  check_coefs_matrix(coefs_obj)
  UseMethod("get_coefs")
}

#' Get dimensions of a vector, matrix, `rescomp_coefs_vector` object or  `rescomp_coefs_matrix` object
#'
#' Provides generic implementations of `length()`/`nrow()`/`ncol()`/`dim()` for objects which can be used as coefficients by `rescomp`.
#' `length()` is only valid on vectors.
#' `nrow()` and `ncol()` are only valid on matrices.
#' `dim()` is valid on either.
#'
#' @param coefs_obj A numeric vector, numeric matrix, or an object of class `rescomp_coefs_vector` or `rescomp_coefs_matrix`.
#'
#' @returns An integer vector, of length 1 for `length()`, `nrow()`, or `ncol()`, or of length 1 or 2 (for vectors or matrices) for `dim()`.
#' @noRd
get_coefs_length <- function(coefs_obj) {
  check_coefs_vector(coefs_obj)
  return(get_coefs_dim(coefs_obj))
}

#' @rdname get_coefs_length
#' @noRd
get_coefs_nrow <- function(coefs_obj) {
  check_coefs_matrix(coefs_obj)
  return(get_coefs_dim(coefs_obj)[1])
}

#' @rdname get_coefs_length
#' @noRd
get_coefs_ncol <- function(coefs_obj) {
  check_coefs_matrix(coefs_obj)
  return(get_coefs_dim(coefs_obj)[2])
}

#' @rdname get_coefs_length
#' @noRd
get_coefs_dim <- function(coefs_obj) {
  UseMethod("get_coefs_dim")
}

#' @export
get_coefs.numeric <- function(coefs_obj, params) {
  return(coefs_obj)
}

#' @export
get_coefs.rescomp_coefs_custom <- function(coefs_obj, params) {
  coefs <- coefs_obj$func(params)
  check_coefs(coefs, get_coefs_dim(coefs_obj), glue::glue("`func` of `{class(coefs_obj)[[1]]}`"))
  return(coefs)
}

#' @export
get_coefs.rescomp_coefs <- function(coefs_obj, params) {
  return(coefs_obj$func(params))
}

#' @export
get_coefs_dim.numeric <- function(coefs_obj) {
  return(length(coefs_obj))
}

#' @export
get_coefs_dim.matrix <- function(coefs_obj) {
  return(dim(coefs_obj))
}

#' @export
get_coefs_dim.rescomp_coefs <- function(coefs_obj) {
  return(coefs_obj$dim)
}

#' Check whether an object is a valid argument to `get_coefs_vector()`/`get_coefs_matrix()`
#'
#' `get_coefs_vector()`/`get_coefs_matrix()` can take either a raw numeric vector/matrix, or a `rescomp_coefs_vector`/`rescomp_coefs_matrix`.
#' These functions check whether an object is a valid argument to `get_coefs_vector()`/`get_coefs_matrix()` respectively.
#'
#' @param coefs_obj An object to check.
#'
#' @returns A logical.
#' @noRd
is_coefs_vector <- function(coefs_obj) {
  return((is.vector(coefs_obj) & is.numeric(coefs_obj)) | "rescomp_coefs_vector" %in% class(coefs_obj))
}

#' @rdname is_coefs_vector
#' @noRd
is_coefs_matrix <- function(coefs_obj) {
  return((is.matrix(coefs_obj) & is.numeric(coefs_obj)) | "rescomp_coefs_matrix" %in% class(coefs_obj))
}

#' Verify that an object is a valid argument to `get_coefs_vector()`/`get_coefs_matrix()`
#'
#' `get_coefs_vector()`/`get_coefs_matrix()` can take either a raw numeric vector/matrix, or a `rescomp_coefs_vector`/`rescomp_coefs_matrix`.
#' These functions verify that an object is a valid argument to `get_coefs_vector()`/`get_coefs_matrix()` respectively.
#' Throws a user-readable error if necessary.
#'
#' @param coefs_obj An object to check.
#'
#' @returns NULL; this function is called for its side effects (throwing an error if necessary).
#' @noRd
check_coefs_vector <- function(coefs_obj, arg = rlang::caller_arg(coefs_obj), call = rlang::caller_env()) {
  if (!is_coefs_vector(coefs_obj)) {
    cli::cli_abort(c(
      "{.arg {arg}} must be a vector or `rescomp_coefs_vector`."
    ), call = call)
  }
  return(invisible(NULL))
}

#' @rdname check_coefs_vector
#' @noRd
check_coefs_matrix <- function(coefs_obj, arg = rlang::caller_arg(coefs_obj), call = rlang::caller_env()) {
  if (!is_coefs_matrix(coefs_obj)) {
    cli::cli_abort(c(
      "{.arg {arg}} must be a matrix or `rescomp_coefs_matrix`."
    ), call = call)
  }
  return(invisible(NULL))
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
check_coefs <- function(obj, dims, func_name, dims_desc = NULL, call = rlang::caller_env()) {
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
      "x" = "`length(dims)` was {length(dims)}."
    ))
  }

  if (!check_func(obj)) {
    cli::cli_abort(c(
      "{func_name} must return {type_article} {expected_type}.",
      "x" = "It returned a {class(obj)[[1]]}."
    ), call = call)
  }
  if (!is.numeric(obj)) {
    cli::cli_abort(c(
      "{func_name} must return a numeric {expected_type}.",
      "x" = "It returned a {mode(obj)} {expected_type}."
    ), call = call)
  }
  if (!identical(get_coefs_dim(obj), as.integer(dims))) {
    if (is.null(dims_desc)) {
      dims_desc <- ""
    } else {
      dims_desc <- glue::glue(" (i.e. {paste('`', dims_desc, '`', sep = '', collapse = ' by ')})")
    }
    if (length(dims) == 1) {
      dims_noun <- "length"
      dims_verb <- "was"
    } else {
      dims_noun <- "dimensions"
      dims_verb <- "were"
    }
    cli::cli_abort(c(
      "{func_name} must return a {expected_type} matching expected {dims_noun}.",
      "x" = "It returned a {expected_type} with {dims_noun} {toString(get_coefs_dim(obj))}.",
      "i" = "Expected {dims_noun} {dims_verb} {toString(dims)}{dims_desc}."
    ), call = call)
  }

  return(invisible(NULL))
}

#' Check that two vectors/matrices or `rescomp_coefs_vector`/`rescomp_coefs_matrix` objects have the same dimensions
#'
#' By default, checks that both objects are of the same type (both vectors/`rescomp_coefs_vector` or both matrices/`rescomp_coefs_matrix`) and that both have identical dimensions.
#' If `check_dims` is provided, it can be made to only check that they are of equal size in certain dimensions.
#' Throws a user-readable error if necessary.
#'
#' @param obj1,obj2 Vectors, matrices, or objects of class `rescomp_coefs_vector` or `rescomp_coefs_matrix` to be checked against one another.
#' @param check_dims A vector of the indices of dimensions to check. For vectors, only dimension 1 is valid. For matrices, dimensions 1 and 2 are valid.
#'
#' @returns NULL; this function is called for its side effects (throwing an error if necessary).
#' @noRd
#'
#' @examples
#'
#' check_coefs_coordinate(1:4, 2:5)
#' try(check_coefs_coordinate(1:4, 1:9))
#' try(check_coefs_coordinate(1:4, matrix(1:9, nrow = 3, ncol = 3)))
#' try(check_coefs_coordinate(matrix(1:4, nrow = 2, ncol = 2), matrix(1:9, nrow = 3, ncol = 3)))
#' check_coefs_coordinate(matrix(1:4, nrow = 2), matrix(1:8, nrow = 2), check_dims = 1)
#' try(check_coefs_coordinate(matrix(1:4, nrow = 2), matrix(1:8, nrow = 2), check_dims = 2))
check_coefs_coordinate <- function(obj1, obj2, check_dims = NULL, arg1 = rlang::caller_arg(obj1), arg2 = rlang::caller_arg(obj2), call = rlang::caller_env()) {
  if (!((is_coefs_vector(obj1) && is_coefs_vector(obj2)) || (is_coefs_matrix(obj1) && is_coefs_matrix(obj2)))) {
    cli::cli_abort(c(
      "{.arg {arg1}} and {.arg {arg2}} must either both be vectors, or both be matrices."
    ), call = call)
  }

  dims1 <- get_coefs_dim(obj1)
  dims2 <- get_coefs_dim(obj2)
  dim_indices <- 1:length(dims1)
  if (is.null(check_dims)) {
    check_dims <- dim_indices
  } else if (!all(check_dims %in% dim_indices)) {
    cli::cli_abort(c(
      "Indices in {.arg check_dims} must be among {toString(dim_indices)}."
    ))
  }

  if (!identical(dims1[check_dims], dims2[check_dims])) {
    # TODO: In the event that check_dims != NULL, this error message ought to be far more explicit about which dimensions are being checked, in a user-friendly way.
    cli::cli_abort(c(
      "Dimensions of {.arg {arg1}} and {.arg {arg2}} must match."
    ), call = call)
  }

  return(invisible(NULL))
}
