#' Generate a Monod (type 2) functional response
#'
#' @param x vector of resource concentrations
#' @param r numeric
#' @param k numeric
#'
#' @return vector
#' @export
#'
#' @examples
#' monod_sim(seq(1,10,0.1), r = 1, k = 0.1)
monod_sim <- function(x, r, k){
  r*x/(k+x)
}
