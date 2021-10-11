#' Generate ggplot friendly data frame for plotting functional responses
#'
#' @param pars S3 object of class `rescomp` detailing model parameters and
#'     specifications.
#' @param maxx Numeric. Maximum resource value to get percapita growth rates across
#'     (for plotting)
#' @param madj Logical. Standardize by mortality. Default = FALSE.
#'
#' @return data frame
#' @export
#'
#' @examples
#' pars <- spec_rescomp()
#' df_funcresp(pars)
#' @importFrom rlang .data
df_funcresp <- function(pars, maxx, madj = FALSE) {
  if (missing(maxx)) {
    resource.levels <- seq(0, 1, length.out = 1000)
  } else {
    stopifnot(is.numeric(maxx), maxx >= 0)
    resource.levels <- seq(0, maxx, length.out = 1000)
  }

  resp.iter <- data.frame(
    matrix(
      nrow = length(resource.levels),
      ncol = pars$nresources
    )
  )
  resp.iter$resource.levels <- resource.levels
  resp.list <- list()
  resp.list.TDP <- list()

  maxparmlen <- max(
    length(pars$mu),
    length(pars$Ks),
    length(pars$Qs),
    length(pars$all_d)
  )

  for (k in 1:maxparmlen) {
    for (i in 1:pars$nconsumers) {
      for (j in 1:pars$nresources) {
        resp.iter[, j] <- func_form(
          R = resource.levels,
          if (length(pars$mu) == 1) {
            mu <- pars$mu[[1]][i, j]
          } else {
            mu <- pars$mu[[k]][i, j]
          },
          if (length(pars$Ks) == 1) {
            mu <- pars$Ks[[1]][i, j]
          } else {
            mu <- pars$Ks[[k]][i, j]
          },
          if (length(pars$all_d) == 1) {
            if (madj == TRUE) {
              mort <- pars$all_d[[1]][i]
            } else {
              mort <- 0
            }
          } else {
            if (madj == TRUE) {
              mort <- pars$all_d[[k]][i]
            } else {
              mort <- 0
            }
          },
          phi = pars$phi[i, j],
          type3 = pars$type3[i, j],
          eff = pars$eff[i, j]
        )
        names(resp.iter)[j] <- paste0(
          "Resource ",
          letters[c(1:pars$nresources)[j]]
        )
      }
      resp.iter$sp <- paste0("N", c(1:pars$nconsumers)[i])
      resp.iter$paramstate <- paste0("env-state-", k)
      resp.list[[i]] <- resp.iter
    }
    resp.list.TDP[[k]] <- do.call("rbind", resp.list)
  }
  resp.all <- do.call("rbind", resp.list.TDP)
  tidyr::pivot_longer(resp.all,
    cols = !c(
      .data$resource.levels,
      .data$sp,
      .data$paramstate
    ),
    names_to = "resource", values_to = "growth"
  )
}
