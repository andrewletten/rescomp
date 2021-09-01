#' Internal function for making time dependent parameters
#'
#' @param crpars Focal parameter
#' @param tpinterp
#'
#' @return A list of approximation functions
#' @export
#'
#' @examples
make_tdpars <- function(crpars){
  if (tpinterp == "inst" | tpinterp == "lin"){
    forcetime <- seq(0, totaltime, timeparfreq)
    cr_funs_byres <- list()
    cr_funs_bycons <- list()

    if (tpinterp == "inst"){
      interpmethod <- "constant"
    } else if (tpinterp == "lin"){
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

  } else if (tpinterp == "sine"){
    cr_funs_byres <- list()
    cr_funs_bycons <- list()
    timeseq <- seq(0, totaltime, 0.1)
    # list of approx functions
    for (i in seq_len(nrow(pars[[crpars]][[1]]))) {
      for (j in seq_len(ncol(pars[[crpars]][[1]]))) {
        amplitude <- (pars[[crpars]][[1]][i, j] - pars[[crpars]][[2]][i, j])/2
        meancr <- (pars[[crpars]][[1]][i, j] + pars[[crpars]][[2]][i, j])/2
        wave <- (meancr + amplitude*sin((2*pi*timeseq)/(timeparfreq)/2))
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
