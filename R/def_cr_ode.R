#' Define consumer resource ODE function
#'
#' @param Time Time to simulate over
#' @param State Vector of initial states
#' @param Pars A list
#'
#' @return Model formula to pass to sim_rescomp
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

    if(timepars == TRUE & length(mu) > 1){
      for (i in seq_len(nrow(mu[[1]]))) {
        for (j in seq_len(ncol(mu[[1]]))) {
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

    Ks_list_live <-  list()
    Ks_live_eachres <-  list()

    if(timepars == TRUE & length(Ks) > 1){
      for (i in seq_len(nrow(Ks[[1]]))) {
        for (j in seq_len(ncol(Ks[[1]]))) {
          Ks_live_eachres[j] <- Ks_approx_fun[[i]][[j]](Time)
        }
        Ks_list_live[[i]] <- Ks_live_eachres
      }
      Ks <- matrix(unlist(Ks_list_live),
                   nrow = nrow(Ks[[1]]),
                   byrow = TRUE)
    } else{
      Ks <- Ks[[1]]
    }

    Qs_list_live <-  list()
    Qs_live_eachres <-  list()

    if(timepars == TRUE & length(Qs) > 1){
      for (i in seq_len(nrow(Qs[[1]]))) {
        for (j in seq_len(ncol(Qs[[1]]))) {
          Qs_live_eachres[j] <- Qs_approx_fun[[i]][[j]](Time)
        }
        Qs_list_live[[i]] <- Qs_live_eachres
      }
      Qs <- matrix(unlist(Qs_list_live),
                   nrow = nrow(Qs[[1]]),
                   byrow = TRUE)
    } else{
      Qs <- Qs[[1]]
    }

    mort_list_live <-  list()
    mort_live_eachres <-  list()

    if(timepars == TRUE & length(all_d) > 1){
      for (i in seq_along(all_d[[1]])) {
        mort_list_live[[i]] <- mort_approx_fun[[i]](Time)
        }
      all_d <- unlist(mort_list_live)
    } else{
      all_d <- all_d[[1]]
    }

    # --------------------------------------------------------------------


    # Consumer dynamics
    dN.perR <- mu # matrix(pars$mu[,1:length(R)])
    dN <- N
    for (i in seq_along(N)) {
      for (j in seq_along(R)) {
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
      for (j in seq_along(R)) {
        for (i in seq_along(N)) {
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
      for (j in seq_along(R)) {
        for (i in seq_along(N)) {
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
    for (j in seq_along(R)) {
      if (Pars$batchtrans == TRUE){
        R[j] <- R[j]*(1-Pars$mortpulse) + Pars$respulse*(Pars$mortpulse)
      } else {
        R[j] <- R[j] + Pars$respulse
      }
    }
    for (i in seq_along(N)) {
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
    for (j in seq_along(R)) {
      R[j] <- R[j]
      }
    for (i in seq_along(N)) {
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
