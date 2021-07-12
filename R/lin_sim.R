#' Generate a linear (type 1) functional response
#'
#' @param x vector of resource concentrations
#' @param r constant of proportionality
#'
#' @return vector of per capita growth rates
#' @export
#'
#' @examples
#' lin_sim(seq(1,10,0.1), r = 1)
lin_sim <- function(x, r){
  r*x
}
