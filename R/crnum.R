#' Check the dimensions of a vector/matrix match expected spnum and resnum
#'
#' @param vec A numeric vector, which is expected to be of length spnum/resnum, or of length 1, in which case it is recycled.
#' @param mat A numeric matrix, which is expected to have spnum rows and resnum columns.
#' @param spnum,resnum Numeric vectors of length 1: the expected number of rows and columns of the given object.
#'
#' @returns Returns vec or mat, recycled to the appropriate dimensions if necessary.
#' @noRd
#'
#' @examples
#' # TODO
enforce_crnum <- function(mat, spnum, resnum, call = rlang::caller_env()) {
  if (nrow(mat) != spnum || ncol(mat) != resnum) {
    cli::cli_abort(c(
      "Matrix dimensions do not match spnum and resnum.",
      "x" = "Matrix had {nrow(mat)} row{?s} and {ncol(mat)} columns{?s}.",
      "i" = "Expected {spnum} (spnum) row{?s} and {resnum} (resnum) columns{?s}."
    ), call = call)
  } else {
    return(mat)
  }
}

#' @rdname enforce_crnum
#' @noRd
enforce_cnum <- function(vec, spnum, call = rlang::caller_env()) {
  if (length(vec) == 1) {
    return(rep(vec, times = spnum))
  } else if (length(vec) != spnum) {
    cli::cli_abort(c(
      "Vector length does not match spnum.",
      "x" = "Vector had length {length(vec)}.",
      "i" = "Expected length {spnum} (spnum)."
    ), call = call)
  } else {
    return(vec)
  }
}

#' @rdname enforce_crnum
#' @noRd
enforce_rnum <- function(vec, resnum, call = rlang::caller_env()) {
  if (length(vec) == 1) {
    return(rep(vec, times = resnum))
  } else if (length(vec) != resnum) {
    cli::cli_abort(c(
      "Vector length does not match resnum.",
      "x" = "Vector had length {length(vec)}.",
      "i" = "Expected length {resnum} (resnum)."
    ), call = call)
  } else {
    return(vec)
  }
}

#' Propagate `spnum` and `resnum` down from `spec_rescomp()` to objects passed to it
#'
#' @param obj An object to be adjusted to the correct `spnum` and `resnum`.
#' @param spnum,resnum Numeric vectors of length 1, to set the `spnum` and `resnum` of `obj`
#'     and its children to.
#'
#' @returns `obj`, adjusted to the correct `spnum` and `resnum`.
#' @noRd
#'
#' @examples
#' # TODO
propagate_crnum <- function(obj, spnum, resnum) {
  UseMethod("propagate_crnum")
}

#' @export
propagate_crnum.default <- function(obj, spnum, resnum) {
  return(obj)
}

#' @export
propagate_crnum.matrix <- function(obj, spnum, resnum) {
  return(enforce_crnum(obj, spnum, resnum))
}

#' @rdname propagate_crnum
#' @noRd
propagate_cnum <- function(obj, spnum) {
  UseMethod("propagate_cnum")
}

#' @export
propagate_cnum.default <- function(obj, spnum) {
  return(obj)
}

#' @export
propagate_cnum.numeric <- function(obj, spnum) {
  return(enforce_cnum(obj, spnum))
}

#' @export
propagate_cnum.integer <- function(obj, spnum) {
  return(enforce_cnum(obj, spnum))
}

#' @rdname propagate_crnum
#' @noRd
propagate_rnum <- function(obj, resnum) {
  UseMethod("propagate_rnum")
}

#' @export
propagate_rnum.default <- function(obj, resnum) {
  return(obj)
}

#' @export
propagate_rnum.numeric <- function(obj, resnum) {
  return(enforce_rnum(obj, resnum))
}

#' @export
propagate_rnum.integer <- function(obj, resnum) {
  return(enforce_rnum(obj, resnum))
}
