#' Generate list of parameters for a consumer-resource model to be passed to `deSolve::ode`
#'
#' @param spnum Number of consumers
#' @param resnum Number of resources
#' @param mumatrix A list containing the matrix(s) of maximum growth rates (type 2) /
#' constant of proportionalities (type 1). The number
#' of rows should be equal to `spnum` and `resnum` respectively. If `timepars` = TRUE,
#' expects a list of length 2.
#' @param kmatrix Matrix of half saturation constants (type 2, ignored if linear = TRUE).
#' The number of rows should be equal to `spnum` and `resnum` respectively.
#' @param qmatrix Matrix of resource quotas. The number of rows should be equal to `spnum` and `resnum` respectively.
#' @param linear If FALSE equals type 2 function (i.e. Monod).
#' @param chemo If FALSE resources grow logistically.
#' @param essential If FALSE resources are substitutable.
#' @param mort Density independent mortality rate.
#' @param resspeed Resource intrinsic rate of increase (if chemo = FALSE), otherwise chemostat dilution rate. Set to zero for pulsing only.
#' @param resconc Resource carrying capacity (if chemo = FALSE), otherwise chemostat supply concentration
#' @param respulse Resource pulse size. Requires events argument in call to `ode`.
#' @param timepars If TRUE, time dependent parameters required.
#' @param timeswitch Frequency of parameter switching if timepars = TRUE.
#' @param timeswitch_length If timepars = TRUE, total time for parameter switching. Should be equivalent to total simulation time.
#'
#' @return list
#' @export
#'
#' @importFrom stats approxfun
#'
#' @examples
#' make_par_list()
make_par_list <-  function(spnum = 1,
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
                           respulse = 0,
                           timepars = FALSE,
                           timeswitch,
                           timeswitch_length){

  # Check input classes
  stopifnot(is.numeric(spnum))
  stopifnot(is.numeric(resnum))
  stopifnot(is.logical(linear))
  stopifnot(is.logical(essential))
  stopifnot(is.logical(chemo))
  stopifnot(is.logical(timepars))
  stopifnot(is.numeric(mort))
  stopifnot(is.numeric(resspeed))
  stopifnot(is.numeric(resconc))
  stopifnot(is.numeric(respulse))
  # mumatrix, kmatrix and qmatrix checks in code block below (as usually missing)
  #

  pars  <-  list()
  if (missing(mumatrix)){
    pars$mu <-  list(matrix(rep(0.1, times = spnum*resnum), nrow = spnum, byrow = TRUE))
  } else{
    stopifnot(is.list(mumatrix))
    pars$mu <- mumatrix
  }
  if (timepars == TRUE){
    # stopifnot(is.numeric(timeswitch))
    # stopifnot(is.numeric(timeswitch_length))
    if (length(pars$mu) == 1) stop(paste0("Time dependent parameters set to true but only one mu matrix provided"))
    forcetime <- seq(0, timeswitch_length, timeswitch)
    mu_funs_byres <-  list()
    mu_funs_bycons <-  list()

    # Create a list of functions
    for (i in 1:nrow(pars$mu[[1]])) {
      for (j in 1:ncol(pars$mu[[1]])) {
        force_mu <- rep(c(pars$mu[[1]][i,j], pars$mu[[2]][i,j]), length.out = length(forcetime))
        mu_funs_byres[[j]] <- approxfun(forcetime, force_mu, method = "constant", rule = 2)
      }
      mu_funs_bycons[[i]] <- mu_funs_byres
    }
    pars$mu_approx_fun = mu_funs_bycons
    pars$timeswitch = timeswitch
  } else {
    if (length(pars$mu) > 1) stop(paste0("Time dependent parameters set to FALSE but more than one mu matrix provided"))
  }
  if (nrow(pars$mu[[1]]) != spnum) stop(paste0("mumatrix(s) should have ", spnum, " rows for ", spnum, " consumers."))
  if (ncol(pars$mu[[1]]) != resnum) stop(paste0("mumatrix(s) should have ", resnum, " columns for ", resnum, " resources."))
  if (missing(kmatrix)){
    pars$Ks <-  matrix(rep(0.1, times = spnum*resnum), nrow = spnum, byrow = TRUE)
  } else{
    stopifnot(is.matrix(kmatrix))
    pars$Ks <- kmatrix
  }
  if (missing(qmatrix)){
    pars$Qs <- matrix(rep(0.001, times = spnum*resnum), nrow = spnum, byrow = TRUE)
  } else {
    stopifnot(is.matrix(qmatrix))
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
  pars$timepars = timepars

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
      "Model properties: \n",
      " * ", spnum, " consumer(s) and ", resnum, " resource(s)\n",
      " * ", "Consumers have", ifelse(linear, " type 1", " type 2"), " functional responses\n",
      " * ", "Resources are", ifelse(essential, " essential", " substitutable"), " (ignore if only a single resource)\n",
      " * ", resdyn, "\n",
      " * ", "Mortality is continuous (equal to resource dilution rate?)\n",
      " * ", ifelse(timepars, paste0("Parameters are time dependent with switching every ", timeswitch, " time steps"),
             "Parameters are constant through time ")
  )
  return(pars)
}
