test_that("cr sim plots look as expected", {
  library(deSolve)
  pars <- suppressMessages(spec_rescomp())
  happenings <- time_vals()
  m1 <- ode(
    func = def_cr_ode,
    y = initiate_state(pars),
    parms = pars,
    times = happenings$totaltime,
    method = "lsoda"
  )
  vdiffr::expect_doppelganger("Default plot as expected", plot_crsim(m1, pars))
})
