#' Generate list of parameters for consumer-resource model
#'
#' @param spnum number of consumers
#' @param resnum number of resources
#' @param mumatrix matrix of maximum growth rates (type 2)/ constant of proportionalities (type 1). # rows = spnum; # cols = resnum
#' @param kmatrix matrix of half saturation constants (type 2, ignored if linear = TRUE). # rows = spnum; # cols = resnum
#' @param qmatrix matrix of resource quotas. # rows = spnum; # cols = resnum
#' @param linear if FALSE equals type 2 function (i.e. Monod)
#' @param chemo if FALSE resources grow logistically
#' @param essential if FALSE resources are substitutable
#' @param mort density independent mortality rate
#' @param resspeed resource intinsic rate of increase (if chemo = FALSE), otherwise chemostat dilution rate. Set to zero for pulsing only.
#' @param resconc resource carrying capcity (if chemo = FALSE), otherwise chemostat supply concentration
#' @param respulse resource pulse size. Requires events argument in call to `ode`.
#'
#' @return list
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
                           chemo = FALSE,
                           mort = 0.03,
                           resspeed = 1,
                           resconc = 1,
                           respulse = 0){
  pars  <-  list()

  if (missing(mumatrix)){
    pars$mu <-  matrix(rep(0.1, times = spnum*resnum), nrow = spnum, byrow = TRUE)
  } else{
    pars$mu <- mumatrix
  }
  if (nrow(pars$mu) != spnum) stop(paste0("mumatrix should have ", spnum, " rows for ", spnum, " consumers."))
  if (ncol(pars$mu) != resnum) stop(paste0("mumatrix should have ", resnum, " columns for ", resnum, " resources."))
  if (missing(kmatrix)){
    pars$Ks <-  matrix(rep(1, times = spnum*resnum), nrow = spnum, byrow = TRUE)
  } else{
    pars$Ks <- kmatrix
  }
  if (missing(qmatrix)){
    pars$Qs <- matrix(rep(0.001, times = spnum*resnum), nrow = spnum, byrow = TRUE)
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
  pars$respulse = respulse
  pars$essential = essential
  pars$chemo = chemo
  pars$nconsumers = spnum
  pars$nresources = resnum
  pars$resspeed = rep(resspeed, times = resnum)
  pars$resconc = rep(resconc, times = resnum)

  resdyn <- paste0(
    if (chemo == TRUE & resspeed != 0 & respulse != 0){
      "Resource supply is continuous (e.g. chemostat) AND pulsed"
    } else if (chemo == TRUE & resspeed == 0 & respulse != 0){
      "Resource supply is pulsed only"
    } else if (chemo == TRUE & resspeed != 0 & respulse == 0){
      "Resource supply is continuous (e.g. chemostat)"
    } else if (chemo == TRUE & resspeed == 0 & respulse == 0){
      "Resources are not supplied?!"
    } else if (chemo == FALSE & resspeed != 0 & respulse != 0){
      "Resources grow logistically and are pulsed"
    } else if (chemo == FALSE & resspeed == 0 & respulse != 0){
      "Resources are pulsed only"
    } else if (chemo == FALSE & resspeed != 0 & respulse == 0){
      "Resources grow logistically"
    } else if (chemo == FALSE & resspeed == 0 & respulse == 0){
      "Resources are not supplied?!"
    }
  )
  message(
    paste0(
      "Model properties: \n", spnum, " consumer(s) and ", resnum, " resource(s)\n",
      "Consumers have", ifelse(linear, " type 1", " type 2")), " functional responses\n",
    "Resources are", ifelse(essential, " essential", " substitutable\n"),
    resdyn, "\n",
    "Mortality is continuous")
  return(pars)
}
