library(deSolve)

test_that("Sim corresponds to manual spec with deSolve", {

  # -------------------------------------------------------------------------
  pars <- suppressMessages(spec_rescomp(totaltime = 50))
  msim <- sim_rescomp(pars)
  # direct in deSolve
  mcheck <- ode(
    func = function(Time, State, Pars) {
      with(as.list(c(State, Pars)), {
        dN <- (N * 0.1 * R) - 0.03 * N
        dR <- 0.03 * (1 - R) - (N * 0.1 * R * 0.001)
        return(list(c(dN, dR)))
      })
    },
    parms = NULL,
    y = c(N = 10, R = 1),
    times = (seq(0.1, 50, 0.1)),
    method = "lsoda"
  )
  colnames(mcheck)[2:3] <- c("1", "2")
  expect_equal(msim, mcheck)
  # -------------------------------------------------------------------------

  # -------------------------------------------------------------------------
  pars <- suppressMessages(spec_rescomp(spnum = 2,
                                totaltime = 50))
  msim <- sim_rescomp(pars)
  # direct in deSolve
  mcheck <- ode(
    func = function(Time, State, Pars) {
      with(as.list(c(State, Pars)), {
        dN1 <- (N1 * 0.1 * R) - 0.03 * N1
        dN2 <- (N2 * 0.1 * R) - 0.03 * N2
        dR <- 0.03 * (1 - R) - (N1 * 0.1 * R * 0.001) - (N2 * 0.1 * R * 0.001)
        return(list(c(dN1, dN2, dR)))
      })
    },
    parms = NULL,
    y = c(N1 = 10, N2 = 10, R = 1),
    times = (seq(0.1, 50, 0.1)),
    method = "lsoda"
  )
  colnames(mcheck)[2:4] <- c("1", "2", "3")
  expect_equal(msim, mcheck, ignore_attr = TRUE)
  # -------------------------------------------------------------------------

  # -------------------------------------------------------------------------
  pars <- suppressMessages(spec_rescomp(resnum = 2,
                                totaltime = 50))
  msim <- sim_rescomp(pars)
  # direct in deSolve
  mcheck <- ode(
    func = function(Time, State, Pars) {
      with(as.list(c(State, Pars)), {
        dN <- (N1 * 0.1 * R1) + (N1 * 0.1 * R2) - 0.03 * N1
        dR1 <- 0.03 * (1 - R1) - (N1 * 0.1 * R1 * 0.001)
        dR2 <- 0.03 * (1 - R2) - (N1 * 0.1 * R2 * 0.001)
        return(list(c(dN, dR1, dR2)))
      })
    },
    parms = NULL,
    y = c(N1 = 10, R1 = 1, R2 = 1),
    times = (seq(0.1, 50, 0.1)),
    method = "lsoda"
  )
  colnames(mcheck)[2:4] <- c("1", "2", "3")
  expect_equal(msim, mcheck)
  # -------------------------------------------------------------------------

  # -------------------------------------------------------------------------
  pars <- suppressMessages(spec_rescomp(funcresp = "type2",
                                totaltime = 50))
  msim <- sim_rescomp(pars)
  # direct in deSolve
  mcheck <- ode(
    func = function(Time, State, Pars) {
      with(as.list(c(State, Pars)), {
        dN <- (N * 0.1 * R)/(1 + R) - 0.03 * N
        dR <- 0.03 * (1 - R) - (N * 0.1 * R * 0.001)/(1 + R)
        return(list(c(dN, dR)))
      })
    },
    parms = NULL,
    y = c(N = 10, R = 1),
    times = (seq(0.1, 50, 0.1)),
    method = "lsoda"
  )
  colnames(mcheck)[2:3] <- c("1", "2")
  expect_equal(msim, mcheck)
  # -------------------------------------------------------------------------

  # -------------------------------------------------------------------------
  pars <- suppressMessages(spec_rescomp(essential = TRUE,
                                totaltime = 50))
  msim <- sim_rescomp(pars)
  # direct in deSolve
  mcheck <- ode(
    func = function(Time, State, Pars) {
      with(as.list(c(State, Pars)), {
        dN <- (N * 0.1 * R) - 0.03 * N
        dR <- 0.03 * (1 - R) - (N * 0.1 * R * 0.001)
        return(list(c(dN, dR)))
      })
    },
    parms = NULL,
    y = c(N = 10, R = 1),
    times = (seq(0.1, 50, 0.1)),
    method = "lsoda"
  )
  colnames(mcheck)[2:3] <- c("1", "2")
  expect_equal(msim, mcheck)
  # -------------------------------------------------------------------------

  # -------------------------------------------------------------------------

  pars <- suppressMessages(spec_rescomp(chemo = TRUE,
                                totaltime = 50))
  msim <- sim_rescomp(pars)
  # direct in deSolve
  mcheck <- ode(
    func = function(Time, State, Pars) {
      with(as.list(c(State, Pars)), {
        dN <- (N * 0.1 * R) - 0.03 * N
        dR <- 0.03 * (1 - R) - (N * 0.1 * R * 0.001)
        return(list(c(dN, dR)))
      })
    },
    parms = NULL,
    y = c(N = 10, R = 1),
    times = (seq(0.1, 50, 0.1)),
    method = "lsoda"
  )
  colnames(mcheck)[2:3] <- c("1", "2")
  expect_equal(msim, mcheck)
  # -------------------------------------------------------------------------

  # -------------------------------------------------------------------------
  pars <- suppressMessages(spec_rescomp(chemo = TRUE,
                               respulse = 1,
                               pulsefreq = 10,
                               totaltime = 50))
  msim <- sim_rescomp(pars)
  # direct in deSolve
  mcheck <- ode(
    func = function(Time, State, Pars) {
      with(as.list(c(State, Pars)), {
        dN <- (N * 0.1 * R) - 0.03 * N
        dR <- 0.03 * (1 - R) - (N * 0.1 * R * 0.001)
        return(list(c(dN, dR)))
      })
    },
    parms = NULL,
    y = c(N = 10, R = 1),
    times = round(seq(0.1, 50, 0.1), 1),
    method = "lsoda",
    events = list(
      func = function(Time, State, Pars) {
        with(as.list(State), {
          N <- N
          R <- R + 1
          return(c(N, R))
        })
      },
      time = seq(10, 50, 10)
  ))
  colnames(mcheck)[2:3] <- c("1", "2")
  expect_equal(msim, mcheck)
  # -------------------------------------------------------------------------

  # -------------------------------------------------------------------------

  # suppressMessages(spec_rescomp(chemo = TRUE,
  #                              resspeed = 0,
  #                              respulse = 1))
  # -------------------------------------------------------------------------

  # -------------------------------------------------------------------------

  # suppressMessages(spec_rescomp(chemo = FALSE,
  #                              resspeed = 0,
  #                              respulse = 1))
  # -------------------------------------------------------------------------

  # -------------------------------------------------------------------------

  # suppressMessages(spec_rescomp(chemo = FALSE,
  #                              resspeed = 0,
  #                              respulse = 0))
  # suppressMessages(spec_rescomp(chemo = FALSE,
  #                              resspeed = 1,
  #                              respulse = 0))
  # suppressMessages(spec_rescomp(chemo = TRUE,
  #                              resspeed = 0,
  #                              respulse = 0))
  # suppressMessages(spec_rescomp(chemo = TRUE,
  #                              resspeed = 1,
  #                              respulse = 0))
  # suppressMessages(spec_rescomp(chemo = FALSE,
  #                              resspeed = 1,
  #                              respulse = 1))
  # suppressMessages(spec_rescomp(batchtrans = TRUE))
  # suppressMessages(spec_rescomp(mortpulse = 0.5))
  # suppressMessages(spec_rescomp(mortpulse = 0.5,
  #                              respulse = 1))
  # suppressMessages(spec_rescomp(mortpulse = 0.5,
  #                              mort = 0))
  # suppressMessages(spec_rescomp(mort = 0))


  })

test_that("Overide defaults raises correct warning", {
})
