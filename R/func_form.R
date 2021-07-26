#' Generate type 1,2 or 3 functional response
#'
#' @param R vector of resource concentrations
#' @param mu maximum growth rate
#' @param Ks half saturation constant
#' @param phi 0 if linear, 1 otherwise
#' @param type3 1 if type 3, 1/2 otherwise
#'
#' @return vector
#' @export
#'
func_form <- function(R, mu, Ks, phi, type3) {
  mu * (R)^(2 * type3) / ((Ks)^(2 * type3) + phi * (R)^(2 * type3))
}
