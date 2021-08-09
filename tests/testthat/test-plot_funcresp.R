test_that("Func resp plots look as expected", {
  pars <- suppressMessages(make_par_list())
  vdiffr::expect_doppelganger("Default funcresp plot", plot_funcresp(pars))
})
