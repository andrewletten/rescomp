#' Generate list of parameters for consumer-resource model
#'
#' @param spnum number of consumers
#' @param resnum number of resources
#' @param mumatrix matrix of maximum growth rates (type 2)/ constant of proportionalities (type 1). # rows = spnum; # cols = resnum
#' @param kmatrix matrix of half saturation constants (type 2, ignored if linear = TRUE). # rows = spnum; # cols = resnum
#' @param qmatrix matrix of resource quotas. # rows = spnum; # cols = resnum
#' @param linear if FALSE equals type 2 function (i.e. Monod)
#' @param essential if FALSE resources are substitutable
#' @param mort density independent mortality rate
#'
#' @return
#' @export
#'
#' @examples
#' make_par_list()
make_par_list <-  function(spnum = 2,
                           resnum = 1,
                           mumatrix,
                           kmatrix,
                           qmatrix,
                           linear = TRUE,
                           essential = FALSE,
                           mort = 0.03){
  pars  <-  list()
  if (missing(mumatrix)){
    pars$mu <-  matrix(rep(0.1, times = spnum*resnum), nrow = spnum, byrow = TRUE)
  } else{
    pars$mu <- mumatrix
  }
  if (missing(kmatrix)){
    pars$Ks <-  matrix(rep(1, times = spnum*resnum), nrow = spnum, byrow = TRUE)
  } else{
    pars$Ks <- kmatrix
  }
  if (missing(qmatrix)){
    pars$Qs <- pars$mu
  } else {
    pars$Qs <- qmatrix
  }
  if(linear == TRUE){
    pars$phi <-  matrix(rep(0, times = spnum*resnum), nrow = spnum, byrow = TRUE)
  } else {
    pars$phi <- matrix(rep(1, times = spnum*resnum), nrow = spnum, byrow = TRUE)
  }
  pars$type3 <- matrix(rep(1/2, times = spnum*resnum), nrow = spnum, byrow = TRUE)
  pars$all_d <- mort

  # below not yet updated (12.07.21)
  pars$nconsumers = spnum
  pars$res.speed = 50 # resource supply rate (chemo) # in original submission this was set to 10 (sped up for consistent pop sizes compared to logistic growth)
  pars$S = rep(1, times = resnum) # resource concentration (chemo) / carrying capacity (logis)
  pars$r.res = rep(100, times = resnum) # resource intrinsic rate of increase (logis)
  pars$cc = pars$S # logis
  pars$spnum = spnum
  pars$resnum = resnum
  return(pars)
}
