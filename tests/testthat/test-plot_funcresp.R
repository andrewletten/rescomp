test_that("Func resp plots look as expected", {
  pars <- suppressMessages(spec_rescomp())
  vdiffr::expect_doppelganger("Default funcresp plot", plot_funcresp(pars))
})
