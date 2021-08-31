#' Define consumer resource ODE function
#'
#' @param Time time to simulate over
#' @param State vector of initial states
#' @param Pars a list
#'
#' @return model formula to pass to ode
#' @export
#'
# #' @examples
def_cr_ode <- function(Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    N <- State[1:nconsumers]
    R <- State[(1 + nconsumers):length(State)]

    # Time dependent parameters ------------------------------------------
    mu_list_live <-  list()
    mu_live_eachres <-  list()

    if(timepars == TRUE){
      for (i in 1:nrow(mu[[1]])) {
        for (j in 1:ncol(mu[[1]])) {
          mu_live_eachres[j] <- mu_approx_fun[[i]][[j]](Time)
        }
        mu_list_live[[i]] <- mu_live_eachres
      }
      mu <- matrix(unlist(mu_list_live),
                   nrow = nrow(mu[[1]]),
                   byrow = TRUE)
    } else{
      mu <- mu[[1]]
    }
    # --------------------------------------------------------------------


    # Consumer dynamics
    dN.perR <- mu # matrix(pars$mu[,1:length(R)])
    dN <- N
    for (i in 1:length(N)) {
      for (j in 1:length(R)) {
        dN.perR[i, j] <- (mu[i, j] * N[i] * (R[j])^(2 * type3[i, j])) /
          ((Ks[i, j])^(2 * type3[i, j]) + phi[i, j] * (R[j])^(2 * type3[i, j]))
      }
      if (Pars$essential == TRUE) {
        dN[i] <- (min(dN.perR[i, ]) * eff[i,j]) - (all_d[i] * N[i])
      } else {
        dN[i] <- sum(dN.perR[i,] * eff[i,j]) - (all_d[i] * N[i])
      }
    }

    # Resource dynamics
    dR.perN <- mu
    dR <- R
    if (Pars$essential == TRUE) {
      for (j in 1:length(R)) {
        for (i in 1:length(N)) {
          dR.perN[i, ] <- (min(dN.perR[i, ])) * Qs[i, ]
        }
        if (Pars$chemo == TRUE) {
          dR[j] <- resspeed[j] * (resconc[j] - R[j]) -
            sum(dR.perN[, j])
        } else {
          dR[j] <- (resspeed[j] * R[j] * (1 - (R[j] / resconc[j]))) -
            sum(dR.perN[, j])
        }
      }
    } else {
      for (j in 1:length(R)) {
        for (i in 1:length(N)) {
          dR.perN[i, j] <- dN.perR[i, j] * Qs[i, j]
        }
        if (Pars$chemo == TRUE) {
          dR[j] <- resspeed[j] * (resconc[j] - R[j]) - sum(dR.perN[, j])
        } else {
          dR[j] <- (resspeed[j] * R[j] * (1 - (R[j] / resconc[j]))) -
            sum(dR.perN[, j])
        }
      }
    }
    return(list(c(dN, dR)))
  })
}


#' Event for resource pulsing
#'
#' @param Time time to simulate over
#' @param State vector of initial states
#' @param Pars a list
#'
# #' @return
#' @export
#'
# #' @examples
eventfun_respulse <- function(Time, State, Pars) {
  with(as.list(State), {
    R <- State[(1 + Pars$nconsumers):length(State)]
    N <- State[1:Pars$nconsumers]
    for (j in 1:length(R)) {
      if (Pars$batchtrans == TRUE){
        R[j] <- R[j]*(1-Pars$mortpulse) + Pars$respulse*(Pars$mortpulse)
      } else {
        R[j] <- R[j] + Pars$respulse
      }
    }
    for (i in 1:length(N)) {
      N[i] <- N[i]*(1-Pars$mortpulse)
    }
    return(c(N, R))
  })
}

#' Event for different consumer start times
#'
#' @param Time time to simulate over
#' @param State vector of initial states
#' @param Pars a list
#'
# #' @return
#' @export
#'
# #' @examples
eventfun_starttime <- function(Time, State, Pars) {
  with(as.list(State), {
    R <- State[(1 + Pars$nconsumers):length(State)]
    N <- State[1:Pars$nconsumers]
    for (j in 1:length(R)) {
      R[j] <- R[j]
      }
    for (i in 1:length(N)) {
      if(Time %in% Pars$introseq[i]){
        N[i] <- Pars$cinit[i]
      } else {
        N[i] <- N[i]
      }
      }
    return(c(N, R))
  })
}


#' Timings of happenings
#'
#' @param total Total time.
#' @param step Step size.
#' @param doround Round time units (handles issues with numerical differences
#'     that produce warning messages when pulsing resources and/or consumers).
#' @param pulse Pulsing interval.
#' @param introseq sequence as vector for consumer introductions.
#'     Vector length must equal spnum.
#'
# #' @return
#' @export
#'
#' @examples
#'
#' time_vals(1000, pulse = 100)
time_vals <- function(total = 1000,
                      step = 0.1,
                      doround = TRUE,
                      pulse,
                      introseq = NULL) {
  time_vals <- list()
  ifelse(doround,
    time_vals$totaltime <- round(seq(0, total, by = step), 1), # sapply(step, nchar) - 2
    time_vals$totaltime <- seq(0, total, by = step)
  )

  if (missing(pulse)) {
    time_vals$pulseseq <- NULL
  } else if (length(pulse) == 1){
    time_vals$pulseseq <- round(seq(pulse, total, by = pulse), 1)
  } else {
    time_vals$pulseseq <- round(pulse, 1)
  }

#  if (!is.null(introseq)){
    time_vals$introseq <- introseq
#  }

  return(time_vals)
}
