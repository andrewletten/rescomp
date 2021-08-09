#' Generate type 1, 2 or 3 functional response
#'
#' @param R Vector of resource concentrations.
#' @param mu Maximum growth rate
#' @param Ks Half saturation constant; 1 if linear (default).
#' @param phi Dummy variable for nonlinear (type I or II) functional responses;
#'    0 if linear (default), 1 otherwise.
#' @param type3 1 if type 3, 1/2 otherwise (default)
#'
#' @return vector
#' @export
#'
#' @examples
#'
#' # Linear (type I) functional response.
#' func_form(R = seq(0, 1, length.out = 1000),
#'           mu = 1,
#'           Ks = 1,
#'           phi = 0,
#'           type3 = 1/2)
#'
#' # Saturating (type II, aka Monod) functional response.
#' func_form(R = seq(0, 1, length.out = 1000),
#'           mu = 1,
#'           Ks = 0.1,
#'           phi = 1,
#'           type3 = 1/2)
#'
#' # Sigmoidal (type III) functional response.
#' func_form(R = seq(0, 1, length.out = 1000),
#'           mu = 1,
#'           Ks = 0.1,
#'           phi = 1,
#'           type3 = 1)
#'
func_form <- function(R, mu, Ks = 1, phi = 0, type3 = 1/2) {
  (mu*(R)^(2*type3)) /
    ((Ks)^(2*type3) + (phi*(R)^(2*type3)))
}
