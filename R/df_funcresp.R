#' Generate ggplot friendly data frame for plotting functional responses
#'
#' @param pars Parameter list from spec_rescomp()
#' @param maxx Maximum resource value to get percapita growth rates across
#'     (for plotting)
#' @param madj Logical. Standardize by mortality. Default = FALSE.
#'
#' @return data frame
#' @export
#'
#' @examples
#' pars <- spec_rescomp()
#' df_funcresp(pars)
#'
#' @importFrom rlang .data
df_funcresp <- function(pars, maxx, madj = FALSE) {
  if (missing(maxx)) {
    resource.levels <- seq(0, 1, length.out = 1000)
  } else {
    stopifnot(is.numeric(maxx), maxx >= 0)
    resource.levels <- seq(0, maxx, length.out = 1000)
  }

  resp.iter <- data.frame(
    matrix(nrow = length(resource.levels),
           ncol = pars$nresources)
    )
  resp.iter$resource.levels <- resource.levels
  resp.list <- list()
  resp.list.TDP <- list()

  for (k in seq_along(pars$mu)) {
    for (i in 1:pars$nconsumers) {
      for (j in 1:pars$nresources) {
        resp.iter[, j] <- func_form(
          R = resource.levels,
          mu = pars$mu[[k]][i, j],
          Ks = pars$Ks[i, j],
          phi = pars$phi[i, j],
          type3 = pars$type3[i, j],
          eff = pars$eff[i, j],
          if(madj == TRUE){
            mort = pars$all_d[i]
          } else {
            mort = 0
          }
        )
        names(resp.iter)[j] <- paste0("Resource ",
                                      letters[c(1:pars$nresources)[j]])
      }
      resp.iter$sp <- paste0("N", c(1:pars$nconsumers)[i])
      resp.iter$paramstate <- paste0("env-state-", k)
      resp.list[[i]] <- resp.iter
    }
    resp.list.TDP[[k]] <- do.call("rbind", resp.list)
  }
  resp.all <- do.call("rbind", resp.list.TDP)
  tidyr::pivot_longer(resp.all,
                      cols = !c(.data$resource.levels,
                                .data$sp,
                                .data$paramstate),
                      names_to = "resource", values_to = "growth")
}
