#' Generate list of parameters for a consumer-resource model to be
#'     passed to `sim_rescomp()`
#'
#' @param spnum Integer vector of length 1: the number of consumers.
#' @param resnum Integer vector of length 1: the number of resources.
#' @param funcresp An object of class `rescomp_funcresp` specifying the functional response.
#' @param quota Numeric matrix or `rescomp_coefs_matrix`, the elements of which
#'     give the resource quotas. The number of rows and columns should be equal to
#'     `spnum` and `resnum` respectively.
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
#' @return S3 object of class `rescomp`.
#' @export
#'
#' @examples
#' # TODO
spec_rescomp <- function(spnum = 1,
                         resnum = 1,
                         funcresp = funcresp_type1(matrix(0.1, nrow = spnum, ncol = resnum)),
                         quota = matrix(0.001, nrow = spnum, ncol = resnum),
                         essential = FALSE,
                         mort = 0.03,
                         ressupply = ressupply_chemostat(0.03, 1),
                         params = rescomp_param_list(),
                         events = list(),
                         totaltime = 1000,
                         cinit = 10,
                         rinit = 1,
                         verbose = TRUE) {
  pars <- list(
    spnum = spnum,
    resnum = resnum,
    funcresp = funcresp,
    quota = quota,
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

  class(pars) <- "rescomp"
  if (verbose == TRUE) {
    print(pars)
  }

  invisible(pars)
}

# TODO: print.rescomp
