#' Internal function for making time dependent parameters
#'
#' @param crpars Focal parameter
#' @param pars Immature pars list
#'
#' @return A list of approximation functions
#' @export
#'
make_tdpars <- function(crpars, pars){
  if (pars$tpinterp == "inst" | pars$tpinterp == "lin"){
    forcetime <- seq(0, pars$totaltime, pars$timeparfreq)
    cr_funs_byres <- list()
    cr_funs_bycons <- list()

    if (pars$tpinterp == "inst"){
      interpmethod <- "constant"
    } else if (pars$tpinterp == "lin"){
      interpmethod <- "linear"
    }

    # list of approx functions
    for (i in seq_len(nrow(pars[[crpars]][[1]]))) {
      for (j in seq_len(ncol(pars[[crpars]][[1]]))) {
        force_cr <- rep(c(pars[[crpars]][[1]][i, j],
                          pars[[crpars]][[2]][i, j]),
                        length.out = length(forcetime))
        cr_funs_byres[[j]] <- approxfun(forcetime,
                                        force_cr,
                                        method = interpmethod,
                                        rule = 2)
      }
      cr_funs_bycons[[i]] <- cr_funs_byres
    }
    cr_approx_fun <- cr_funs_bycons

  } else if (pars$tpinterp == "sine"){
    cr_funs_byres <- list()
    cr_funs_bycons <- list()
    timeseq <- seq(0, pars$totaltime, 0.1)
    # list of approx functions
    for (i in seq_len(nrow(pars[[crpars]][[1]]))) {
      for (j in seq_len(ncol(pars[[crpars]][[1]]))) {
        amplitude <- (pars[[crpars]][[1]][i, j] - pars[[crpars]][[2]][i, j])/2
        meancr <- (pars[[crpars]][[1]][i, j] + pars[[crpars]][[2]][i, j])/2
        wave <- (meancr + amplitude*sin((2*pi*timeseq)/(pars$timeparfreq)/2))
        cr_funs_byres[[j]] <- approxfun(timeseq,
                                        wave,
                                        method = "linear",
                                        rule = 2)
      }
      cr_funs_bycons[[i]] <- cr_funs_byres
    }
    cr_approx_fun <- cr_funs_bycons
  }
  cr_approx_fun
}
