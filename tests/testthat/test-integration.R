# Integration tests -------------------------------------------------------
library(deSolve)

# -------------------------------------------------------------------------
test_that("defaults as expected", {
  # rescomp approach
  pars <- suppressMessages(make_par_list())
  happenings <- time_vals(50)
  m1 <- ode(
    func = def_cr_ode,
    y = initiate_state(vars = pars),
    parms = pars,
    times = happenings$totaltime,
    method = "lsoda"
  )

  # direct in deSolve
  mcheck <- ode(
    func = function(Time, State, Pars) {
      with(as.list(c(State, Pars)), {
        dN <- (N * 0.1 * R) - 0.03 * N
        dR <- 1 * R * (1 - R / 1) - (N * 0.1 * R * 0.001)
        return(list(c(dN, dR)))
      })
    },
    parms = NULL,
    y = c(N = 10, R = 1),
    times = (seq(0.1, 50, 0.1)),
    method = "lsoda"
  )

  colnames(mcheck)[2:3] <- c("1", "2")
  expect_equal(m1, mcheck)
})


# -------------------------------------------------------------------------
test_that("defaults with chemo TRUE as expected", {
  # rescomp approach
  pars <- suppressMessages(make_par_list(chemo = TRUE))
  happenings <- time_vals(50)
  m1 <- ode(
    func = def_cr_ode,
    y = initiate_state(vars = pars),
    parms = pars,
    times = happenings$totaltime,
    method = "lsoda"
  )

  # direct in deSolve
  mcheck <- ode(
    func = function(Time, State, Pars) {
      with(as.list(c(State, Pars)), {
        dN <- (N * 0.1 * R) - 0.03 * N
        dR <- 1 * (1 - R) - (N * 0.1 * R * 0.001)
        return(list(c(dN, dR)))
      })
    },
    parms = NULL,
    y = c(N = 10, R = 1),
    times = (seq(0.1, 50, 0.1)),
    method = "lsoda"
  )

  colnames(mcheck)[2:3] <- c("1", "2")
  expect_equal(m1, mcheck)
})


# -------------------------------------------------------------------------

test_that("2 type II on single logistic as expected", {
  # rescomp approach
  pars <- suppressMessages(make_par_list(
    spnum = 2,
    resnum = 1,
    linear = FALSE,
    mumatrix = list(matrix(c(0.7, 0.05),
                           nrow = 2,
                           ncol = 1,
                           byrow = TRUE
    )),
    kmatrix = matrix(c(2, 0.015),
                     nrow = 2,
                     ncol = 1,
                     byrow = TRUE
    ),
    resspeed = 3,
    resconc = 0.2
  ))

  happenings <- time_vals(50)

  m1 <- ode(
    func = def_cr_ode,
    y = initiate_state(vars = pars),
    parms = pars,
    times = happenings$totaltime,
    method = "lsoda"
  )

  # direct in deSolve
  mcheck <- ode(
    func = function(Time, State, Pars) {
      with(as.list(c(State, Pars)), {
        dN1 <- (N1*0.7*R) / (2 + R) - 0.03*N1
        dN2 <- (N2*0.05*R) / (0.015 + R) - 0.03*N2
        dR <- 3*R*(1 - R/0.2) -
          (N1*0.7*R*0.001) / (2 + R) -
          (N2*0.05*R*0.001) / (0.015 + R)
        return(list(c(dN1, dN2, dR)))
      })
    },
    parms = NULL,
    y = c(N1 = 10, N2 = 10, R = 0.2),
    times = (seq(0.1, 50, 0.1)),
    method = "lsoda"
  )

  colnames(mcheck)[2:4] <- c("1", "2", "3")
  expect_equal(m1, mcheck)
})

# -------------------------------------------------------------------------

test_that("Two type 1 consumers and two substitutable resources
          in a chemostat as expected", {

  # rescomp approach
  pars <- suppressMessages(make_par_list(
    spnum = 2,
    resnum = 2,
    linear = TRUE,
    mumatrix = list(matrix(c(0.07,0.03,
                             0.04,0.05),
                           nrow = 2,
                           ncol = 2,
                           byrow = TRUE)),
    resspeed = 3,
    resconc = 1,
    chemo = TRUE,
    essential = FALSE
  ))

  happenings <- time_vals(50)

  m1 <- ode(
    func = def_cr_ode,
    y = initiate_state(vars = pars),
    parms = pars,
    times = happenings$totaltime,
    method = "lsoda"
  )

  # direct in deSolve
  mcheck <- ode(
    func = function(Time, State, Pars) {
      with(as.list(c(State, Pars)), {
        dN1 <- (N1*0.07*R1) + (N1*0.03*R2) - 0.03*N1
        dN2 <- (N2*0.04*R1) + (N2*0.05*R2) - 0.03*N2
        dR1 <- 3*(1 - R1) -
          (N1*0.07*R1*0.001) -
          (N2*0.04*R1*0.001)
        dR2 <- 3*(1 - R2) -
          (N1*0.03*R2*0.001) -
          (N2*0.05*R2*0.001)
        return(list(c(dN1, dN2, dR1, dR2)))
      })
    },
    parms = NULL,
    y = c(N1 = 10, N2 = 10, R1 = 1, R2 = 1),
    times = (seq(0.1, 50, 0.1)),
    method = "lsoda",

  )

  colnames(mcheck)[2:5] <- c("1", "2", "3", "4")
  expect_equal(m1, mcheck, ignore_attr = TRUE)
})

# -------------------------------------------------------------------------

test_that("Two type 2 consumers and one externally pulsed resource
          (continuous mortality)", {

            # rescomp approach
            pars <- suppressMessages(make_par_list(
              spnum = 2,
              resnum = 1,
              linear = FALSE,
              mumatrix = list(matrix(c(0.7,0.05),
                                     nrow = 2,
                                     ncol = 1,
                                     byrow = TRUE)),
              kmatrix = matrix(c(2, 0.015),
                               nrow = 2,
                               ncol = 1,
                               byrow = TRUE),
              resspeed = 0, # set to zero for no additional resource supply
              resconc = 0.2,
              respulse = 0.3 # resource pulse size
            ))

            happenings <- time_vals(50, pulse = 5)

            m1 <- ode(
              func = def_cr_ode,
              y = initiate_state(vars=pars),
              parms = pars,
              times = happenings$totaltime,
              method = "lsoda",
              # events argument takes a list including the event function and a vector of pulsing intervals
              events = list(func = eventfun_respulse, time = happenings$pulseseq),
            )

            # direct in deSolve
            mcheck <- ode(
              func = function(Time, State, Pars) {
                with(as.list(c(State, Pars)), {
                  dN1 <- (N1*0.7*R)/(2+R) - 0.03*N1
                  dN2 <- (N2*0.05*R)/(0.015+R) +  - 0.03*N2
                  dR <- - (N1*0.7*R*0.001)/(2+R) - (N2*0.05*R*0.001)/(0.015+R)
                  return(list(c(dN1, dN2, dR)))
                })
              },
              parms = NULL,
              y = c(N1 = 10, N2 = 10, R = 0.2),
              times = round(seq(0.1, 50, 0.1), 1),
              method = "lsoda",
              events = list(
                func = function(Time, State, Pars) {
                  with(as.list(State), {
                    N1 <- N1
                    N2 <- N2
                    R <- R + 0.3
                    return(c(N1, N2, R))
                    })
                  },
                time = seq(5, 50, 5)
            )
            )

            colnames(mcheck)[2:4] <- c("1", "2", "3")
            expect_equal(m1, mcheck, ignore_attr = TRUE)
          })

# -------------------------------------------------------------------------

test_that("Two type 2 consumers and one externally pulsed resource
          (serial transfer with pulsed mortality)", {

            # rescomp approach
            pars <- suppressMessages(make_par_list(
              spnum = 2,
              resnum = 1,
              linear = FALSE,
              mumatrix = list(matrix(c(0.2,0.2),
                                     nrow = 2,
                                     ncol = 1,
                                     byrow = TRUE)),
              kmatrix = matrix(c(0.3, 0.2),
                               nrow = 2,
                               ncol = 1,
                               byrow = TRUE),
              chemo = TRUE,
              resspeed = 0,
              resconc = 1,
              respulse = 1,
              mort = 0,
              mortpulse = 0.8,
              batchtrans = TRUE
            ))

            happenings <- time_vals(total = 50, pulse = 10)

            m1 <- ode(
              func = def_cr_ode,
              y = initiate_state(restart = 1, vars=pars),
              parms = pars,
              times = happenings$totaltime,
              method = "lsoda",
              events = list(func = eventfun_respulse, time = happenings$pulseseq)
            )

            # direct in deSolve
            mcheck <- ode(
              func = function(Time, State, Pars) {
                with(as.list(c(State, Pars)), {
                  dN1 <- (N1*0.2*R)/(0.3+R)
                  dN2 <- (N2*0.2*R)/(0.2+R)
                  dR <- - (N1*0.2*R*0.001)/(0.3+R) - (N2*0.2*R*0.001)/(0.2+R)
                  return(list(c(dN1, dN2, dR)))
                })
              },
              parms = NULL,
              y = c(N1 = 10, N2 = 10, R = 1),
              times = round(seq(0.1, 50, 0.1), 1),
              method = "lsoda",
              events = list(
                func = function(Time, State, Pars) {
                  with(as.list(State), {
                    N1 <- N1*0.2
                    N2 <- N2*0.2
                    R <- R*(1 - 0.8) + 0.8*1
                    return(c(N1, N2, R))
                  })
                },
                time = seq(10, 50, 10)
              )
            )

            colnames(mcheck)[2:4] <- c("1", "2", "3")
            expect_equal(m1, mcheck, ignore_attr = TRUE)
          })

# -------------------------------------------------------------------------

# test_that("Two type 2 consumers with time dependent consumption parameters and
#           one continuously supplied resource)", {
#           })
