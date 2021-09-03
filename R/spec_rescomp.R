#' Generate list of parameters for a consumer-resource model to be
#'     passed to `sim_rescomp`
#'
#' @param spnum Integer vector of length 1 for the number of consumers
#' @param resnum Integer vector of length 1 for the number of resources
#' @param mumatrix Numeric matrix or list of matrices, the elements of which
#'     give the maximum growth rates (type 2 or 3) or resource consumption rate
#'     constants (type 1, or type 2 when parameterised using conversion
#'     efficiencies with `effmatrix` as opposed to resource quotas with
#'     `qmatrix`). The number of rows and columns should be equal to `spnum`
#'     and `resnum` respectively. For time dependent parameters
#'     (with `timepars` = TRUE), expects a list of length 2.
#' @param kmatrix Numeric matrix or list of matrices, the elements of which
#'     give the of half saturation constants (type 2 or type 3, ignored if
#'     funcresp = "type1"). The number of rows and columns should be equal to
#'     `spnum` and `resnum` respectively. For time dependent parameters
#'     (with `timepars` = TRUE), expects a list of length 2.
#' @param qmatrix Numeric matrix or list of matrices, the elements of which
#'     give the resource quotas. The number of rows and columns should be equal to
#'     `spnum` and `resnum` respectively. For time dependent parameters
#'     (with `timepars` = TRUE), expects a list of length 2.
#'     Default = 0.001 for all consumers.
#' @param effmatrix Numeric matrix of resource conversion efficiencies.
#'     The number of rows should be equal to `spnum` and `resnum` respectively.
#'     NB. Incompatible with specification of resource quotas. Function will
#'     throw an error if both specified.
#' @param funcresp Character vector of length 1 or length equal to `spnum`.
#'     Options include "type1", 'type2", or "type3".
#' @param chemo Logical. Default is resources supplied continuously (chemostat).
#'     If FALSE resources grow logistically.
#' @param essential Logical vector of length 1. If FALSE resources are substitutable.
#' @param mort Numeric vector or list of vectors of length 1 or
#'     length = `spnum`, specifying density independent mortality rates.
#'     For time dependent parameters
#'     (with `timepars` = TRUE), expects a list of length 2.
#' @param resspeed Numeric vector of length 1 specifying resource intrinsic
#'     rate of increase (if chemo = FALSE), or otherwise chemostat dilution
#'     rate. Set to zero for pulsing only. For continuous dilution of resource
#'     without resource supply, `resspeed` should be non-zero with `resconc`
#'     set to zero.
#' @param resconc Numeric vector of length 1 or length = `resnum` specifying
#'     resource carrying capacity (if chemo = FALSE), or otherwise chemostat
#'     supply concentration
#' @param respulse Resource pulse size.
#' @param mortpulse Consumer mortality fraction if non-constant mortality
#'     (e.g. serial-batch transfer).
#' @param pulsefreq Frequency of resource pulsing and/or intermittent mortality.
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
#' @param cinit Numeric vector of length 1 or length = `spnum` specifying
#'     initialconsumer state values (densities). Defaults to 10 for all
#'     consumers.
#' @param rinit Numeric vector of length 1 or length = `resnum` specifying
#'     initial resource state values (concentrations). Defaults to value given
#'     in `resconc`.
#' @param introseq Time sequence as a vector for consumer introductions.
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
#'     mumatrix = matrix(c(0.7,0.3,
#'                         0.4,0.5),
#'                         nrow = 2,
#'                         ncol = 2,
#'                         byrow = TRUE),
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
                         effmatrix = NULL,
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
                         rinit = NULL,
                         introseq = NULL,
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
  stopifnot(is.numeric(mortpulse))
  stopifnot(mortpulse >= 0 & mortpulse <= 1)
  stopifnot(is.numeric(resspeed))
  stopifnot(is.numeric(resconc))
  stopifnot(is.numeric(respulse))
  stopifnot(is.numeric(cinit))

  pars <- list()

  # mu matrix
  if (missing(mumatrix)) {
    pars$mu <- list(
      matrix(rep(0.1, times = spnum * resnum),
             nrow = spnum,
             byrow = TRUE))
  } else {
    stopifnot(is.list(mumatrix) | is.matrix(mumatrix))
    pars$mu <- mumatrix
  }

  if (!is.list(pars$mu)) pars$mu = list(pars$mu)

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

  # kmatrix
  if (missing(kmatrix)) {
    pars$Ks <- list(matrix(rep(1, times = spnum * resnum),
                      nrow = spnum,
                      byrow = TRUE))
  } else {
    stopifnot(is.matrix(kmatrix) | is.list(kmatrix))
    pars$Ks <- kmatrix
  }

  if (!is.list(pars$Ks)) pars$Ks <- list(pars$Ks)

  # qmatrix/effmatrix
  if (missing(qmatrix) & is.null(effmatrix)) {
    pars$Qs <- matrix(rep(0.001, times = spnum * resnum),
                      nrow = spnum,
                      byrow = TRUE)
    pars$eff <- matrix(rep(1, times = spnum * resnum),
                       nrow = spnum,
                       byrow = TRUE)
  } else if (!missing(qmatrix) & is.null(effmatrix)){
    stopifnot(is.matrix(qmatrix) | is.list(qmatrix))
    pars$Qs <- qmatrix
    pars$eff <- matrix(rep(1, times = spnum * resnum),
                       nrow = spnum,
                       byrow = TRUE)
  } else if (!missing(qmatrix) & !is.null(effmatrix)){
    stop("Model should be parameterised with resource quotas OR resource
         efficiency, not both.")
  } else if(missing(qmatrix) & !is.null(effmatrix)){
    stopifnot(is.matrix(effmatrix))
    pars$eff <- effmatrix
    pars$Qs <- matrix(rep(1, times = spnum * resnum),
                      nrow = spnum,
                      byrow = TRUE)
  }

  if (!is.list(pars$Qs)) pars$Qs <- list(pars$Qs)

  # mortality
  stopifnot(is.numeric(mort) | is.list(mort))
  if (!is.list(mort)){
    if(length(mort) == 1){
      pars$all_d <- list(rep(mort, times = spnum))
    } else {
      stopifnot(length(mort) == spnum)
      pars$all_d <- list(mort)
    }
  } else {
    if(length(mort[[1]]) == 1){
      pars$all_d <- list(rep(mort[[1]], times = spnum),
                         rep(mort[[2]], times = spnum))
    } else {
      stopifnot(length(mort[[1]]) == spnum)
      pars$all_d <- mort
    }
  }


  pars$totaltime <- totaltime
  pars$tpinterp <- tpinterp

  # time dependent parms
  if (timepars == TRUE) {
    if (length(pars$mu) == 1 &
        length(pars$Ks) == 1 &
        length(pars$Qs) == 1 &
        length(pars$all_d) == 1)


      stop("Time dependent parameters set to true but
        only one mu-, k-, q-matrix and mortality vector provided")
    if (missing(timeparfreq)){
      stop("If timepars = TRUE, timeparfreq must be provided")
    } else {
      pars$timeparfreq <- timeparfreq
    }

    if(length(pars$mu) > 1){
      pars$mu_approx_fun <- make_tdpars("mu", pars)
    }
    if(length(pars$Ks) > 1){
      pars$Ks_approx_fun <- make_tdpars("Ks", pars)
    }
    if(length(pars$Qs) > 1){
      pars$Qs_approx_fun <- make_tdpars("Qs", pars)
    }
    if(length(pars$all_d) > 1){
      pars$mort_approx_fun <- make_tdpars("all_d", pars)
    }


  } else {
    if (length(pars$mu) > 1 | length(pars$Ks) > 1 | length(pars$Qs) > 1)
      stop(paste0(
        "Time dependent parameters set to FALSE but
        more than one mu, k, and/or q matrix provided"))
  }

  # functional response
  if (length(funcresp) == 1){
    if (funcresp == "type1") {
      pars$phi <- matrix(rep(0, times = spnum*resnum),
                         nrow = spnum,
                         byrow = TRUE)
      pars$type3 <- matrix(rep(1/2, times = spnum*resnum),
                           nrow = spnum,
                           byrow = TRUE)
      if (!missing(kmatrix)){
          stop("Matrix of half saturation constants provided
               for a linear (type 1) functional response")
      }

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

  } else if (length(funcresp) > 1){
    if(length(funcresp) != spnum)
    stop("Length of funcresp must equal spnum if a vector (length > 1) of
         functional responses provided")

    pars$phi <- matrix(rep(0, times = spnum*resnum),
                       nrow = spnum,
                       byrow = TRUE)
    pars$type3 <- matrix(rep(1/2, times = spnum*resnum),
                         nrow = spnum,
                         byrow = TRUE)

    for (i in 1:length(funcresp)){
      if (funcresp[i] == "type1"){
        pars$phi[i,] = pars$phi[i,]
        pars$type3[i,] = pars$type3[i,]
        if (length(pars$Ks) == 1){
          pars$Ks[[1]][i,] = 1
          if (any(kmatrix[i,] != 1)){warning(
            paste0(strwrap("Warning: half saturation constant ignored (set to 1) for
                  type 1 functional response", prefix = " ")), "\n\n")}
        } else {
          pars$Ks[[1]][i,] = 1
          pars$Ks[[2]][i,] = 1
          if (any(kmatrix[[1]][i,] != 1) | any(kmatrix[[2]][i,] != 1)){warning(
            paste0(strwrap("Warning: half saturation constant ignored (set to 1) for
                  type 1 functional response", prefix = " ")), "\n\n")}
        }

      } else if (funcresp[i] == "type2"){
        pars$phi[i,] = 1
        pars$type3[i,] = pars$type3[i,]
      } else if (funcresp[i] == "type3"){
        pars$phi[i,] = 1
        pars$type3[i,] = 1
        }
      }
    }


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


  # resconc
  if(length(resconc) == 1){
    pars$resconc <- rep(resconc, times = resnum)
  } else {
    pars$resconc <- resconc
  }

  # rinit
  if(length(rinit) > 1 & resnum != length(rinit))
    stop("Length of rinit must equal resnum if a vector (length > 1) of initial states provided")
  if(is.null(rinit)){
    pars$rinit <- pars$resconc
  } else {
    pars$rinit <- rinit
  }

  pars$respulse <- respulse
  pars$essential <- essential
  pars$chemo <- chemo
  pars$nconsumers <- spnum
  pars$nresources <- resnum
  pars$resspeed <- rep(resspeed, times = resnum)
  pars$timepars <- timepars
  pars$mortpulse <- mortpulse
  pars$pulsefreq <- pulsefreq
  pars$batchtrans <- batchtrans
  pars$funcresp <- funcresp
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
  unifuncresps <- unique(pars$funcresp)
  if (detail == "summary"){
  message(
    "Model properties \n",

    paste0(" * ",
           pars$nconsumers, " consumer(s) and ", pars$nresources, " resource(s)",
           "\n"),

    if (length(unifuncresps) == 1){
      paste0(" * ",
             "Consumers have ",
             gsub("(\\w+)(\\d)", "\\1 \\2", unifuncresps), " functional responses",
             "\n")
    } else if (length(unifuncresps) == 2){
      paste0(" * ",
             "Consumers have ",
             gsub("(\\w+)(\\d)", "\\1 \\2", unifuncresps[1]),
             " or ",
             gsub("(\\w+)(\\d)", "\\1 \\2", unifuncresps[2]),
             " functional responses",
             "\n")

    } else if (length(unifuncresps) == 3){
      paste0(" * ",
             "Consumers have ",
             gsub("(\\w+)(\\d)", "\\1 \\2", unifuncresps[1]),
             " or ",
             gsub("(\\w+)(\\d)", "\\1 \\2", unifuncresps[2]),
             " or ",
             gsub("(\\w+)(\\d)", "\\1 \\2", unifuncresps[3]),
             " functional responses",
             "\n")
    },



    if (pars$nresources > 1) {
      paste0(" * ",
             "Resources are",
             ifelse(pars$essential, " essential\n", " substitutable\n")
      )
    },

    paste0(" * ",
           if (pars$chemo == TRUE & pars$resspeed[1] != 0 & pars$respulse != 0 & pars$resconc[1] != 0) {
             "Resource supply is continuous (e.g. chemostat) AND pulsed"
           } else if(pars$chemo == TRUE & pars$resspeed[1] != 0 & pars$respulse != 0 & pars$resconc[1] == 0){
             "Resource supply is pulsed only (but continuously diluted)"
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

    if (length(pars$all_d) == 1){
      paste0(" * ",
             if(all(pars$all_d[[1]] > 0) & pars$mortpulse == 0){
               "Mortality is continuous"
             } else if (all(pars$all_d[[1]] > 0) & pars$mortpulse > 0){
               "Mortality is continuous and intermittent"
             } else if (all(pars$all_d[[1]] == 0) & pars$mortpulse > 0){
               "Mortality intermittent"
             } else if (all(pars$all_d[[1]] == 0) & pars$mortpulse == 0){
               "No mortality"
             },
             "\n")

    } else if(length(pars$all_d) == 2){
      paste0(" * ",
             if(all(pars$all_d[[1]] > 0) &
                all(pars$all_d[[2]] > 0) &
                pars$mortpulse == 0){
               "Mortality is continuous"
             } else if (all(pars$all_d[[1]] > 0) &
                        all(pars$all_d[[2]] > 0) &
                        pars$mortpulse > 0){
               "Mortality is continuous and intermittent"
             } else if (all(pars$all_d[[1]] == 0) &
                        all(pars$all_d[[2]] == 0) &
                        pars$mortpulse > 0){
               "Mortality intermittent"
             } else if (all(pars$all_d[[1]] == 0) &
                        all(pars$all_d[[2]] == 0) &
                        pars$mortpulse == 0){
               "No mortality"
             },
             "\n")
           },


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
                  paste0(pars$rinit,
                         collapse = ", "), "]"),
           "\n"),

    if(!is.null(pars$introseq)){
      paste0(" * ",
             "Some or all consumers introduced after timepoint 0",
             "\n")
      }
  )
  } else if (detail == "list"){
    print(pars[])
  }
}
