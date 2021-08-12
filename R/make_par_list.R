#' Generate list of parameters for a consumer-resource model to be
#'     passed to `deSolve::ode`
#'
#' @param spnum Number of consumers
#' @param resnum Number of resources
#' @param mumatrix A list containing the matrix(s) of maximum growth rates
#'     (type 2) / constant of proportionalities (type 1).
#'     The number of rows should be equal to `spnum` and `resnum` respectively.
#'     If `timepars` = TRUE, expects a list of length 2.
#' @param kmatrix Matrix of half saturation constants
#'     (type 2, ignored if linear = TRUE). The number of rows should be equal
#'     to `spnum` and `resnum` respectively.
#' @param qmatrix Matrix of resource quotas.
#'     The number of rows should be equal to `spnum` and `resnum` respectively.
#' @param linear If FALSE equals type 2 function (i.e. Monod).
#' @param chemo If FALSE resources grow logistically.
#' @param essential If FALSE resources are substitutable.
#' @param mort Density independent mortality rate.
#' @param resspeed Resource intrinsic rate of increase (if chemo = FALSE),
#'     otherwise chemostat dilution rate. Set to zero for pulsing only.
#' @param resconc Resource carrying capacity (if chemo = FALSE),
#'     otherwise chemostat supply concentration
#' @param respulse Resource pulse size.
#'     Requires events argument in call to `ode`.
#' @param mortpulse Consumer mortality fraction if non-constant mortality
#'     (e.g. serial-batch transfer). Requires events argument in call to `ode`.
#' @param pulsefreq Frequency of resource pulsing and/or intemittent mortality.
#' @param batchtrans If TRUE, both resource is also fractionally sampled
#'     (see vignette).
#' @param timepars If TRUE, time dependent parameters required.
#' @param timeparfreq Frequency of parameter switching if timepars = TRUE.
#' @param totaltime Total simulation time
#' @param timeparfreq Frequency of parameter switching if timepars = TRUE.
#' @param cinit Initial consumer state values (densities). Either a single
#'     integer for all consumers or a vector. Defaults to 10 for all
#'     consumers. Note Initial resource state values defaults to `resconc`.
#'
#'
#' @return list
#' @export
#'
#' @importFrom stats approxfun
#'
#' @examples
#'
#' # Single type I consumer on a single logistically growing resource
#' make_par_list()
#'
#' # Two type II consumers and two substitutable resources in a chemostat
#' pars <- make_par_list(
#'     spnum = 2,
#'     resnum = 2,
#'     linear = FALSE,
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
make_par_list <- function(spnum = 1,
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
                          mortpulse = 0,
                          pulsefreq = 0,
                          batchtrans = FALSE,
                          timepars = FALSE,
                          totaltime = 1000,
                          timeparfreq = 0,
                          cinit = 10) {

  # Check input classes
  stopifnot(is.numeric(spnum))
  stopifnot(is.numeric(resnum))
  stopifnot(is.logical(linear))
  stopifnot(is.logical(essential))
  stopifnot(is.logical(chemo))
  stopifnot(is.logical(timepars))
  stopifnot(is.numeric(mort))
  stopifnot(is.numeric(mortpulse))
  stopifnot(mortpulse >= 0 & mortpulse <= 1)
  stopifnot(is.numeric(resspeed))
  stopifnot(is.numeric(resconc))
  stopifnot(is.numeric(respulse))
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
      stop(paste0(
        "Time dependent parameters set to true but
        only one mu matrix provided"))
    forcetime <- seq(0, totaltime, timeparfreq)
    mu_funs_byres <- list()
    mu_funs_bycons <- list()

    # list of approx functions
    for (i in seq_len(nrow(pars$mu[[1]]))) {
      for (j in seq_len(ncol(pars$mu[[1]]))) {
        force_mu <- rep(c(pars$mu[[1]][i, j],
                          pars$mu[[2]][i, j]),
                        length.out = length(forcetime))
        mu_funs_byres[[j]] <- approxfun(forcetime,
                                        force_mu,
                                        method = "constant",
                                        rule = 2)
      }
      mu_funs_bycons[[i]] <- mu_funs_byres
    }
    pars$mu_approx_fun <- mu_funs_bycons
    pars$timeparfreq <- timeparfreq
  } else {
    if (length(pars$mu) > 1)
      stop(paste0(
        "Time dependent parameters set to FALSE but
        more than one mu matrix provided"))
  }


  if (missing(kmatrix)) {
    pars$Ks <- matrix(rep(1, times = spnum * resnum),
                      nrow = spnum,
                      byrow = TRUE)
  } else {
    stopifnot(is.matrix(kmatrix))
    pars$Ks <- kmatrix
    if (linear == TRUE)
      stop("Matrix of half saturation constants provided
           for a linear (type 1) functional response")
  }

  if (missing(qmatrix)) {
    pars$Qs <- matrix(rep(0.001, times = spnum * resnum),
                      nrow = spnum,
                      byrow = TRUE)
  } else {
    stopifnot(is.matrix(qmatrix))
    pars$Qs <- qmatrix
  }

  if (linear == TRUE) {
    pars$phi <- matrix(rep(0, times = spnum*resnum),
                       nrow = spnum,
                       byrow = TRUE)
  } else {
    pars$phi <- matrix(rep(1, times = spnum*resnum),
                       nrow = spnum,
                       byrow = TRUE)
  }

  pars$type3 <- matrix(rep(1/2, times = spnum*resnum),
                       nrow = spnum,
                       byrow = TRUE) # currently fixed on type 1 or type 2
  pars$all_d <- mort
  pars$respulse <- respulse
  pars$essential <- essential
  pars$chemo <- chemo
  pars$nconsumers <- spnum
  pars$nresources <- resnum
  pars$resspeed <- rep(resspeed, times = resnum)
  pars$resconc <- rep(resconc, times = resnum)
  pars$timepars <- timepars
  pars$mortpulse <- mortpulse
  pars$totaltime <- totaltime
  pars$pulsefreq <- pulsefreq
  pars$batchtrans <- batchtrans
  pars$cinit <- cinit

  # Print model/simulation properites

  message(
    "Model properties \n",

    paste0(" * ",
          spnum, " consumer(s) and ", resnum, " resource(s)",
          "\n"),

    paste0(" * ",
          "Consumers have",
          ifelse(linear, " type 1", " type 2"), " functional responses",
          "\n"),

    if (resnum > 1) {
      paste0(" * ",
              "Resources are",
              ifelse(essential, " essential\n", " substitutable\n")
             )
      },

    paste0(" * ",
           if (chemo == TRUE & resspeed != 0 & respulse != 0) {
             "Resource supply is continuous (e.g. chemostat) AND pulsed"
           } else if (chemo == TRUE & resspeed == 0 & respulse != 0) {
             "Resource supply is pulsed only"
           } else if (chemo == TRUE & resspeed != 0 & respulse == 0) {
             "Resource supply is continuous (e.g. chemostat)"
           } else if (chemo == TRUE & resspeed == 0 & respulse == 0) {
             "Resources are not supplied?!"
           } else if (chemo == FALSE & resspeed != 0 & respulse != 0) {
             "Resources grow logistically and are pulsed"
           } else if (chemo == FALSE & resspeed == 0 & respulse != 0) {
             "Resources are pulsed only"
           } else if (chemo == FALSE & resspeed != 0 & respulse == 0) {
             "Resources grow logistically"
           } else if (chemo == FALSE & resspeed == 0 & respulse == 0) {
             "Resources are not supplied?!"
           },
           "\n"),

    paste0(" * ",
          if(mort > 0 & mortpulse == 0){
            "Mortality is continuous"
          } else if (mort > 0 & mortpulse > 0){
            "Mortality is continuous and intermittent"
          } else if (mort == 0 & mortpulse > 0){
            "Mortality intermittent"
          } else if (mort == 0 & mortpulse == 0){
            "No mortality"
          },
          "\n"),

    paste0(" * ",
          ifelse(timepars,
                 paste0("Parameters are time dependent with switching every ",
                        timeparfreq,
                        " time steps"),
                 "Parameters are constant through time"
                 ),
          "\n",
          "\n"
          ),

    "Simulation properties \n",

    paste0(" * ",
           "Simulation time: ", totaltime, " time steps", "\n"),

    if (respulse != 0 & mortpulse == 0){
      paste0(" * ",
             "Resources pulsing every ", pulsefreq, " timesteps",
             "\n")
      } else if (respulse == 0 & mortpulse != 0){
      paste0(" * ",
             "Intermittent mortality every", pulsefreq, " timesteps",
             "\n")
        } else if (respulse != 0 & mortpulse != 0){
      paste0(" * ",
             "Resources pulsing and intermittent mortality every ", pulsefreq,
             " timesteps",
             "\n")
        } else {

          },

    paste0(" * ",
           "Init state: consumer(s) = ",
           if(length(pars$cinit) == 1 & spnum > 1){
             paste0("[",
                    paste0(rep(cinit, times = spnum),
                           collapse = ", "),
                    "]")
           } else {
             cinit
           },
           ", resource(s) = ",
           paste0("[",
                  paste0(pars$resconc,
                         collapse = ", "), "]"),
           "\n")
  )
  return(pars)
}
