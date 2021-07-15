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
#' @param chemospeed chemostat dilution rate
#' @param chemoconc chemostat supply concentration
#' @param logisK resource carrying capcity (if chemo = FALSE)
#' @param logisr resource intinsic rate of increase (if chemo = FALSE)
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
                           mort = 0.03,
                           chemospeed = 10,
                           chemoconc = 1,
                           logisK,
                           logisr){
  pars  <-  list()

  if (missing(mumatrix)){
    pars$mu <-  matrix(rep(0.1, times = spnum*resnum), nrow = spnum, byrow = TRUE)
  } else{
    pars$mu <- mumatrix
  }
  if (nrow(mumatrix) != spnum) stop(paste0("mumatrix should have ", spnum, " rows for ", spnum, " consumers."))
  if (ncol(mumatrix) != spnum) stop(paste0("mumatrix should have ", resnum, " rows for ", resnum, " resources."))
  if (missing(kmatrix)){
    pars$Ks <-  matrix(rep(1, times = spnum*resnum), nrow = spnum, byrow = TRUE)
  } else{
    pars$Ks <- kmatrix
  }
  if (missing(qmatrix)){
    pars$Qs <- 0.01*pars$mu
  } else {
    pars$Qs <- qmatrix
  }
  if(linear == TRUE){
    pars$phi <-  matrix(rep(0, times = spnum*resnum), nrow = spnum, byrow = TRUE)
  } else {
    pars$phi <- matrix(rep(1, times = spnum*resnum), nrow = spnum, byrow = TRUE)
  }
  pars$type3 <- matrix(rep(1/2, times = spnum*resnum), nrow = spnum, byrow = TRUE) # currently fixed on type 1 or type 2
  pars$all_d <- mort
  pars$nconsumers = spnum
  pars$nresources = resnum
  pars$chemospeed = chemospeed # resource supply rate (chemo)
  pars$chemoconc = rep(chemoconc, times = resnum) # resource concentration (chemo)
  if (missing(logisr)){ # resource intrinsic rate of increase (logis)
    pars$logisr <- rep(0.1, times = resnum)
  } else {
    pars$logisr <- logisr
  }
  if (missing(logisK)){ # resource carrying capacity logis
    pars$logisK <- rep(1, times = resnum)
  } else {
    pars$logisK <- logisK
  }
  return(pars)
}
