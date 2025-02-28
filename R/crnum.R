#' Propagate `spnum` and `resnum` down from `spec_rescomp()` to objects passed to it.
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

propagate_crnum.default <- function(obj, spnum, resnum) {
  return(obj)
}

#' @rdname propagate_crnum
#' @noRd
propagate_cnum <- function(obj, spnum) {
  UseMethod("propagate_cnum")
}

propagate_cnum.default <- function(obj, spnum) {
  return(obj)
}

#' @rdname propagate_crnum
#' @noRd
propagate_rnum <- function(obj, resnum) {
  UseMethod("propagate_rnum")
}

propagate_rnum.default <- function(obj, resnum) {
  return(obj)
}
