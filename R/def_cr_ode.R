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
        dN[i] <- min(dN.perR[i, ]) - (all_d * N[i])
      } else {
        dN[i] <- sum(dN.perR[i, ]) - (all_d * N[i])
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


#' Timings of happenings
#'
#' @param total total time
#' @param step step size
#' @param doround round time vector?
#' @param pulse pulsing interval
#'
# #' @return
#' @export
#'
# #' @examples
time_vals <- function(total = 1000,
                      step = 0.1,
                      doround = TRUE,
                      pulse) {
  time_vals <- list()
  ifelse(doround,
    time_vals$totaltime <- round(seq(0.1, total, by = step), 1),
    time_vals$totaltime <- seq(0.1, total, by = step)
  )
  if (missing(pulse)) {
    time_vals$pulseseq <- NULL
  } else {
    time_vals$pulseseq <- seq(pulse, total, by = pulse)
  }
  return(time_vals)
}
