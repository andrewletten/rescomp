#' Generate list of parameters for a consumer-resource model to be
#'     passed to `sim_rescomp`
#'
#' @param spnum Number of consumers
#' @param resnum Number of resources
#' @param mumatrix A list containing the matrix(s) of maximum growth rates
#'     (type 2 or 3) / constant of proportionalities (type 1).
#'     The number of rows should be equal to `spnum` and `resnum` respectively.
#'     If `timepars` = TRUE, expects a list of length 2.
#' @param kmatrix Matrix of half saturation constants
#'     (type 2 or type 3, ignored if funcresp = "type1). The number of rows should be equal
#'     to `spnum` and `resnum` respectively.
#' @param qmatrix Matrix of resource quotas.
#'     The number of rows should be equal to `spnum` and `resnum` respectively.
#' @param funcresp Options include "type1", 'type2", or "type3".
#' @param chemo Default is resources supplied continuously (chemostat).
#'     If FALSE resources grow logistically.
#' @param essential If FALSE resources are substitutable.
#' @param mort Density independent mortality rate.
#' @param resspeed Resource intrinsic rate of increase (if chemo = FALSE),
#'     otherwise chemostat dilution rate. Set to zero for pulsing only.
#' @param resconc Resource carrying capacity (if chemo = FALSE),
#'     otherwise chemostat supply concentration
#' @param respulse Resource pulse size.
#' @param mortpulse Consumer mortality fraction if non-constant mortality
#'     (e.g. serial-batch transfer).
#' @param pulsefreq Frequency of resource pulsing and/or intemittent mortality.
#' @param batchtrans If TRUE, both resource is also fractionally sampled
#'     (see vignette).
#' @param timepars If TRUE, time dependent parameters required.
#' @param timeparfreq Frequency of parameter switching if timepars = TRUE.
#' @param tpinterp Either "inst" (default), "lin" or "sine" interpolation of
#'     time dependent parameters. If "inst", parameters switch instantaneously
#'     with frequency given by `timeparfreq`. If "lin" or "sine", parameters
#'     are interpolated linearly or sinusoidaly, respectively, with period 2x
#'     `timeparfreq`.
#' @param totaltime Total simulation time
#' @param cinit Initial consumer state values (densities). Either a single
#'     integer for all consumers or a vector. Defaults to 10 for all
#'     consumers. Note initial resource state values defaults to `resconc`.
#' @param introseq Time sequence as vector for consumer introductions.
#'     Vector length must equal spnum.
#' @param verbose If TRUE (default) prints model and simulation summary specs.
#'
#'
#' @return List object of class 'rescomp'.
#' @export
#'
#' @importFrom stats approxfun
#'
#' @examples
#'
#' # Single type I consumer on a single logistically growing resource
#' spec_rescomp()
#'
#' # Two type II consumers and two substitutable resources in a chemostat
#' pars <- spec_rescomp(
#'     spnum = 2,
#'     resnum = 2,
#'     funcresp = "type2",
#'     mumatrix = list(matrix(c(0.7,0.3,
#'                              0.4,0.5),
#'                            nrow = 2,
#'                            ncol = 2,
#'                            byrow = TRUE)),
#'     resspeed = 3,
#'     resconc = 1,
#'     chemo = TRUE,
#'     essential = FALSE
#' )
#'
spec_rescomp <- function(spnum = 1,
                         resnum = 1,
                         mumatrix,
                         kmatrix,
                         qmatrix,
                         funcresp = "type1",
                         essential = FALSE,
                         chemo = TRUE,
                         mort = 0.03,
                         resspeed = 0.03,
                         resconc = 1,
                         respulse = 0,
                         mortpulse = 0,
                         pulsefreq = 0,
                         batchtrans = FALSE,
                         timepars = FALSE,
                         totaltime = 1000,
                         timeparfreq = 0,
                         tpinterp = "inst",
                         cinit = 10,
                         introseq = NULL,
 #                        rinit,
                         verbose = TRUE) {

  # Check input classes
  stopifnot(is.numeric(spnum))
  stopifnot(is.numeric(resnum))
  stopifnot(is.character(funcresp))
  stopifnot(is.character(tpinterp))
  stopifnot(is.logical(essential))
  stopifnot(is.logical(chemo))
  stopifnot(is.logical(timepars))
  stopifnot(is.logical(verbose))
  stopifnot(is.numeric(mort))
  stopifnot(is.numeric(mortpulse))
  stopifnot(mortpulse >= 0 & mortpulse <= 1)
  stopifnot(is.numeric(resspeed))
  stopifnot(is.numeric(resconc))
  stopifnot(is.numeric(respulse))
  stopifnot(is.numeric(cinit))

  # mumatrix, kmatrix and qmatrix checks below

  pars <- list()

  # mu matrix
  if (missing(mumatrix)) {
    pars$mu <- list(
      matrix(rep(0.1, times = spnum * resnum),
             nrow = spnum,
             byrow = TRUE))
  } else {
    stopifnot(is.list(mumatrix))
    pars$mu <- mumatrix
  }
  if (nrow(pars$mu[[1]]) != spnum)
    stop(paste0(
      "mumatrix(s) should have ",
      spnum,
      " rows for ",
      spnum,
      " consumers.")
      )
  if (ncol(pars$mu[[1]]) != resnum)
    stop(paste0(
      "mumatrix(s) should have ",
      resnum,
      " columns for ",
      resnum, "
      resources.")
      )


  # time dependent parms
  if (timepars == TRUE) {
    # stopifnot(is.numeric(timeswitch))
    # stopifnot(is.numeric(timeswitch_length))
    if (length(pars$mu) == 1)
      stop(
        "Time dependent parameters set to true but
        only one mu matrix provided")
    if (missing(timeparfreq))
      stop("If timepars = TRUE, timeparfreq must be provided")

    if (tpinterp == "inst" | tpinterp == "lin"){
      forcetime <- seq(0, totaltime, timeparfreq)
      mu_funs_byres <- list()
      mu_funs_bycons <- list()

      if (tpinterp == "inst"){
        interpmethod <- "constant"
      } else if (tpinterp == "lin"){
        interpmethod <- "linear"
      }

      # list of approx functions
      for (i in seq_len(nrow(pars$mu[[1]]))) {
        for (j in seq_len(ncol(pars$mu[[1]]))) {
          force_mu <- rep(c(pars$mu[[1]][i, j],
                            pars$mu[[2]][i, j]),
                          length.out = length(forcetime))
          mu_funs_byres[[j]] <- approxfun(forcetime,
                                          force_mu,
                                          method = interpmethod,
                                          rule = 2)
        }
        mu_funs_bycons[[i]] <- mu_funs_byres
      }
      pars$mu_approx_fun <- mu_funs_bycons
      pars$timeparfreq <- timeparfreq

    } else if (tpinterp == "sine"){
      mu_funs_byres <- list()
      mu_funs_bycons <- list()
      timeseq <- seq(0, totaltime, 0.1)
      # list of approx functions
      for (i in seq_len(nrow(pars$mu[[1]]))) {
        for (j in seq_len(ncol(pars$mu[[1]]))) {
          amplitude <- (pars$mu[[1]][i, j] - pars$mu[[2]][i, j])/2
          meanmu <- (pars$mu[[1]][i, j] + pars$mu[[2]][i, j])/2
          wave <- (meanmu + amplitude*sin((2*pi*timeseq)/(timeparfreq)/2))
          mu_funs_byres[[j]] <- approxfun(timeseq,
                                          wave,
                                          method = "linear",
                                          rule = 2)
        }
        mu_funs_bycons[[i]] <- mu_funs_byres
      }
      pars$mu_approx_fun <- mu_funs_bycons
      pars$timeparfreq <- timeparfreq

    }

  } else {
    if (length(pars$mu) > 1)
      stop(paste0(
        "Time dependent parameters set to FALSE but
        more than one mu matrix provided"))
  }

  # kmatrix
  if (missing(kmatrix)) {
    pars$Ks <- matrix(rep(1, times = spnum * resnum),
                      nrow = spnum,
                      byrow = TRUE)
  } else {
    stopifnot(is.matrix(kmatrix))
    pars$Ks <- kmatrix
    if (funcresp == "type1")
      stop("Matrix of half saturation constants provided
           for a linear (type 1) functional response")
  }

  # qmatrix
  if (missing(qmatrix)) {
    pars$Qs <- matrix(rep(0.001, times = spnum * resnum),
                      nrow = spnum,
                      byrow = TRUE)
  } else {
    stopifnot(is.matrix(qmatrix))
    pars$Qs <- qmatrix
  }

  # functional response

  if (funcresp == "type1") {
    pars$phi <- matrix(rep(0, times = spnum*resnum),
                       nrow = spnum,
                       byrow = TRUE)
    pars$type3 <- matrix(rep(1/2, times = spnum*resnum),
                         nrow = spnum,
                         byrow = TRUE)

  } else if (funcresp == "type2"){
    pars$phi <- matrix(rep(1, times = spnum*resnum),
                       nrow = spnum,
                       byrow = TRUE)
    pars$type3 <- matrix(rep(1/2, times = spnum*resnum),
                       nrow = spnum,
                       byrow = TRUE)

  } else if (funcresp == "type3"){
    pars$phi <- matrix(rep(1, times = spnum*resnum),
                       nrow = spnum,
                       byrow = TRUE)
    pars$type3 <- matrix(rep(1, times = spnum*resnum),
                         nrow = spnum,
                         byrow = TRUE)
  }


  # if (linear == TRUE) {
  #   pars$phi <- matrix(rep(0, times = spnum*resnum),
  #                      nrow = spnum,
  #                      byrow = TRUE)
  # } else {
  #   pars$phi <- matrix(rep(1, times = spnum*resnum),
  #                      nrow = spnum,
  #                      byrow = TRUE)
  # }
  #
  # # type 3, currently fixed on type 1 or type 2
  # pars$type3 <- matrix(rep(1/2, times = spnum*resnum),
  #                      nrow = spnum,
  #                      byrow = TRUE)

  # cinit
  if(length(cinit) > 1 & spnum != length(cinit))
    stop("Length of cinit must equal spnum if a vector (length > 1) of initial states provided")
  if(length(cinit) == 1){
    pars$cinit <- rep(cinit, times = spnum)
  } else {
    pars$cinit <- cinit
  }

  if(!is.null(introseq) & length(introseq) != spnum){
    stop("Vector of times for consumer introductions must equal spnum")
  }



  # if(length(rinit) > 1 & resnum != length(rinit))
  #   stop("Length of rinit must equal resnum if a vector (length > 1) of initial states provided")
  # pars$rinit <- rinit

  # resconc
  if(length(resconc) == 1){
    pars$resconc <- rep(resconc, times = resnum)
  } else {
    pars$resconc <- resconc
  }

  pars$all_d <- mort
  pars$respulse <- respulse
  pars$essential <- essential
  pars$chemo <- chemo
  pars$nconsumers <- spnum
  pars$nresources <- resnum
  pars$resspeed <- rep(resspeed, times = resnum)
  pars$timepars <- timepars
  pars$mortpulse <- mortpulse
  pars$totaltime <- totaltime
  pars$pulsefreq <- pulsefreq
  pars$batchtrans <- batchtrans
  pars$funcresp <- funcresp
  pars$tpinterp <- tpinterp
  pars$introseq <- introseq


  class(pars) <- "rescomp"
  if(verbose == TRUE){
    print(pars)
  }

  invisible(pars)
}



#' S3 print method for class rescomp
#'
#' @param x rescomp object
#' @param ... Further arguments
#' @param detail Either "summary" or "list". "summary" (default) provides plain language
#' summary info. "list" prints the full parameter list.
#'
#' @export
#'
print.rescomp <- function(x, ..., detail = "summary"){
  pars <-  x
  if (detail == "summary"){
  message(
    "Model properties \n",

    paste0(" * ",
           pars$nconsumers, " consumer(s) and ", pars$nresources, " resource(s)",
           "\n"),

    paste0(" * ",
           "Consumers have ",
           gsub("(\\w+)(\\d)", "\\1 \\2", pars$funcresp), " functional responses",
           "\n"),

    if (pars$nresources > 1) {
      paste0(" * ",
             "Resources are",
             ifelse(pars$essential, " essential\n", " substitutable\n")
      )
    },

    paste0(" * ",
           if (pars$chemo == TRUE & pars$resspeed[1] != 0 & pars$respulse != 0) {
             "Resource supply is continuous (e.g. chemostat) AND pulsed"
           } else if (pars$chemo == TRUE & pars$resspeed[1] == 0 & pars$respulse != 0) {
             "Resource supply is pulsed only"
           } else if (pars$chemo == TRUE & pars$resspeed[1] != 0 & pars$respulse == 0) {
             "Resource supply is continuous (e.g. chemostat)"
           } else if (pars$chemo == TRUE & pars$resspeed[1] == 0 & pars$respulse == 0) {
             "Resources are not supplied?!"
           } else if (pars$chemo == FALSE & pars$resspeed[1] != 0 & pars$respulse != 0) {
             "Resources grow logistically and are pulsed"
           } else if (pars$chemo == FALSE & pars$resspeed[1] == 0 & pars$respulse != 0) {
             "Resources are pulsed only"
           } else if (pars$chemo == FALSE & pars$resspeed[1] != 0 & pars$respulse == 0) {
             "Resources grow logistically"
           } else if (pars$chemo == FALSE & pars$resspeed[1] == 0 & pars$respulse == 0) {
             "Resources are not supplied?!"
           },
           "\n"),

    paste0(" * ",
           if(pars$all_d > 0 & pars$mortpulse == 0){
             "Mortality is continuous"
           } else if (pars$all_d > 0 & pars$mortpulse > 0){
             "Mortality is continuous and intermittent"
           } else if (pars$all_d == 0 & pars$mortpulse > 0){
             "Mortality intermittent"
           } else if (pars$all_d == 0 & pars$mortpulse == 0){
             "No mortality"
           },
           "\n"),

    if(pars$timepars == TRUE){
      if(pars$tpinterp == "inst"){
        paste0(" * ", "Time dependent parameters with instantaneous switching every ",
             pars$timeparfreq,
             " timesteps",
             "\n",
             "\n")
      } else if (pars$tpinterp == "lin"){
        paste0(" * ", "Time dependent parameters with linear interpolation (period = ",
             pars$timeparfreq*2,
             " timesteps)",
             "\n",
             "\n")
      } else if (pars$tpinterp == "sine"){
        paste0(" * ", "Time dependent parameters with sinusoidal interpolation (period = ",
               pars$timeparfreq*2,
               " timesteps)",
               "\n",
               "\n")
      }

    } else {
      "\n"
    },

    "Simulation properties \n",

    paste0(" * ",
           "Simulation time: ", pars$totaltime, " time steps", "\n"),

    if (pars$respulse != 0 & pars$mortpulse == 0){
      paste0(" * ",
             "Resources pulsing every ", pars$pulsefreq, " timesteps",
             "\n")
    } else if (pars$respulse == 0 & pars$mortpulse != 0){
      paste0(" * ",
             "Intermittent mortality every ", pars$pulsefreq, " timesteps",
             "\n")
    } else if (pars$respulse != 0 & pars$mortpulse != 0){
      paste0(" * ",
             "Resources pulsing and intermittent mortality every ", pars$pulsefreq,
             " timesteps",
             "\n")
    } else {

    },

    paste0(" * ",
           "Init state: consumer(s) = ",
           if(length(pars$cinit) == 1 & pars$nconsumers > 1){
             paste0("[",
                    paste0(rep(pars$cinit, times = pars$nconsumers),
                           collapse = ", "),
                    "]")
           } else {
             paste0("[",
                    paste0(pars$cinit, collapse = ", "),
                    "]")
           },
           ", resource(s) = ",
           paste0("[",
                  paste0(pars$resconc,
                         collapse = ", "), "]"),
           "\n")
  )
  } else if (detail == "list"){
    print(pars[])
  }
}
