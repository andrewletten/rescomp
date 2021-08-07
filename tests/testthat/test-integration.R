# Integration tests -------------------------------------------------------
library(deSolve)

# -------------------------------------------------------------------------
test_that("defaults as expected", {
  # rescomp approach
  pars <- make_par_list()
  happenings <- time_vals()
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
    times = (seq(0.1, 1000, 0.1)),
    method = "lsoda"
  )

  colnames(mcheck)[2:3] <- c("1", "2")
  expect_equal(m1, mcheck)
})


# -------------------------------------------------------------------------
test_that("defaults with chemo TRUE as expected", {
  # rescomp approach
  pars <- make_par_list(chemo = TRUE)
  happenings <- time_vals()
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
    times = (seq(0.1, 1000, 0.1)),
    method = "lsoda"
  )

  colnames(mcheck)[2:3] <- c("1", "2")
  expect_equal(m1, mcheck)
})


# -------------------------------------------------------------------------

test_that("2 type II on single logistic as expected", {
  # rescomp approach
  pars <- make_par_list(
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
  )

  happenings <- time_vals()

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
        dN1 <- (N1 * 0.7 * R) / (2 + R) - 0.03 * N1
        dN2 <- (N2 * 0.05 * R) / (0.015 + R) - 0.03 * N2
        dR <- 3 * R * (1 - R / 0.2) - (N1 * 0.7 * R * 0.001) / (2 + R) -
          (N2 * 0.05 * R * 0.001) / (0.015 + R)
        return(list(c(dN1, dN2, dR)))
      })
    },
    parms = NULL,
    y = c(N1 = 10, N2 = 10, R = 0.2),
    times = (seq(0.1, 1000, 0.1)),
    method = "lsoda"
  )

  colnames(mcheck)[2:4] <- c("1", "2", "3")
  expect_equal(m1, mcheck)
})

# -------------------------------------------------------------------------

test_that("Two type 1 consumers and two substitutable resources
          in a chemostat as expected", {

  # rescomp approach
  pars <- make_par_list(
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
  )

  happenings <- time_vals()

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
        dN1 <- (N1 * 0.07 * R1) + (N1 * 0.03 * R2) - 0.03 * N1
        dN2 <- (N2 * 0.04 * R1) + (N2 * 0.05 * R2) - 0.03 * N2
        dR1 <- 3 * (1 - R1) -
          (N1 * 0.07 * R1 * 0.001) -
          (N2 * 0.04 * R1 * 0.001)
        dR2 <- 3 * (1 - R2) -
          (N1 * 0.03 * R2 * 0.001) -
          (N2 * 0.05 * R2 * 0.001)
        return(list(c(dN1, dN2, dR1, dR2)))
      })
    },
    parms = NULL,
    y = c(N1 = 10, N2 = 10, R1 = 1, R2 = 1),
    times = (seq(0.1, 1000, 0.1)),
    method = "lsoda"
  )

  colnames(mcheck)[2:5] <- c("1", "2", "3", "4")
  expect_equal(m1, mcheck, ignore_attr = TRUE)
})
