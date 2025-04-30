#' Generate list of parameters for a consumer-resource model to be
#'     passed to `sim_rescomp()`
#'
#' @param spnum Integer vector of length 1: the number of consumers.
#' @param resnum Integer vector of length 1: the number of resources.
#' @param funcresp An object of class `rescomp_funcresp` specifying the functional response.
#' @param quota Numeric matrix or `rescomp_coefs_matrix`, the elements of which
#'     give the resource quotas. The number of rows and columns should be equal to
#'     `spnum` and `resnum` respectively.
#'     Mutually exclusive with `efficiency`.
#' @param efficiency Numeric matrix or `rescomp_coefs_matrix`, the elements of which
#'     give the efficiency of each consumer on each resource. The number of rows
#'     and columns should be equal to `spnum` and `resnum` respectively.
#'     Mutually exclusive with `quota`.
#' @param essential Logical vector of length 1. If FALSE resources are substitutable.
#' @param mort Numeric vector or `rescomp_coefs_vector` of length `spnum`,
#'     specifying density independent mortality rates.
#' @param ressupply An object of class `rescomp_ressupply` specifying the resource supply.
#' @param params An object of class `rescomp_param_list` specifying a set of parameters which vary
#'     with time, on which other parameters of the model (e.g. funcresp, ressupply) may depend.
#' @param events A list of objects of class `rescomp_event_schedule`, specifying events that
#'     instantaneously change consumer or resource densities.
#' @param totaltime Numeric vector of length 1: the total simulation time.
#' @param cinit Numeric vector of length 1 or length `spnum` specifying
#'     initial consumer state values (densities).
#' @param rinit Numeric vector of length 1 or length `resnum` specifying
#'     initial resource state values (concentrations).
#' @param verbose If TRUE (default) prints model and simulation summary specs.
#'
#' @details
#' Only one of `efficiency` and `quota` should be specified. Specifying both is an error.
#' The default, if neither is specified, is to use `quota`.
#' If using `quota`, the functional responses are taken to give per capita growth rates.
#' If using `efficiency`, the functional responses are taken to give attack rates.
#'
#' @return S3 object of class `rescomp`.
#' @export
#'
#' @examples
#' # TODO
spec_rescomp <- function(spnum = 1,
                         resnum = 1,
                         funcresp = funcresp_type1(crmatrix(0.1)),
                         quota = crmatrix(0.001),
                         efficiency,
                         essential = FALSE,
                         mort = 0.03,
                         ressupply = ressupply_chemostat(0.03, 1),
                         params = rescomp_param_list(),
                         events = list(),
                         totaltime = 1000,
                         cinit = 10,
                         rinit = 1,
                         verbose = FALSE) {
  funcresp <- propagate_crnum(funcresp, spnum, resnum)
  if (!missing(quota) || missing(efficiency)) {
    quota <- propagate_crnum(quota, spnum, resnum)
  }
  if (!missing(efficiency)) {
    efficiency <- propagate_crnum(efficiency, spnum, resnum)
  }
  mort <- propagate_cnum(mort, spnum)
  ressupply <- propagate_crnum(ressupply, spnum, resnum)
  for (i in seq_along(events)) {
    events[[i]] <- propagate_crnum(events[[i]], spnum, resnum)
  }
  cinit <- propagate_cnum(cinit, spnum)
  rinit <- propagate_rnum(rinit, resnum)

  pars <- list(
    spnum = spnum,
    resnum = resnum,
    funcresp = funcresp,
    essential = essential,
    mort = mort,
    ressupply = ressupply,
    params = params,
    events = events,
    event_schedule_df = prepare_event_schedule_df(events, totaltime),
    totaltime = totaltime,
    cinit = cinit,
    rinit = rinit
  )

  if (!missing(quota) && !missing(efficiency)) {
    cli::cli_abort(c(
      "Must not specify both {.arg quota} and {.arg efficiency}."
    ))
  } else if (!missing(efficiency)) {
    pars$efficiency <- efficiency
  } else {
    pars$quota <- quota
  }

  class(pars) <- "rescomp"
  if (verbose == TRUE) {
    print(pars)
  }

  invisible(pars)
}

# TODO: print.rescomp
