test_that("rescomp plots look as expected", {
  library(deSolve)
  pars <- suppressMessages(spec_rescomp())
  m1 <- sim_rescomp(pars)
  vdiffr::expect_doppelganger("Default plot as expected", plot_rescomp(m1))
})
