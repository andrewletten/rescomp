test_that("Func resp plots look as expected", {
  pars <- suppressMessages(spec_rescomp())
  vdiffr::expect_doppelganger("Default funcresp plot",
                              plot_funcresp(pars))

  pars <- suppressMessages(spec_rescomp(
    timepars = TRUE,
    mumatrix = list(matrix(1), matrix(1)),
    timeparfreq = 100))
  vdiffr::expect_doppelganger("Two time states, one resource",
                              plot_funcresp(pars))

  pars <- suppressMessages(spec_rescomp(
    resnum = 2,
    timepars = TRUE,
    mumatrix = list(matrix(c(1,1), ncol = 2), matrix(c(1,1), ncol = 2)),
    timeparfreq = 100))
  vdiffr::expect_doppelganger("Two time states, two resources",
                              plot_funcresp(pars))

  pars <- suppressMessages(spec_rescomp(
    resnum = 2,
    timepars = FALSE,
    mumatrix = list(matrix(c(1,1), ncol = 2)),
    timeparfreq = 100))
  vdiffr::expect_doppelganger("One time states, two resources",
                              plot_funcresp(pars))
})
