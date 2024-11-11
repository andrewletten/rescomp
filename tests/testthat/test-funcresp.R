test_that("spec_funcresp_custom() works", {
  t1 <- spec_funcresp_custom(function(resources, params) { # A type 1 functional response implemented manually.
    params$attack_rate * matrix(rep(resources, each = 2), nrow = 2)
  }, spnum = 2, resnum = 2)

  expect_equal(get_funcresp(t1, 2, c(0.5, 1.5), list(attack_rate = 1.2)), matrix(c(0.6, 1.8, 0.6, 1.8), nrow = 2, byrow = TRUE))
  expect_equal(get_funcresp(t1, 2, c(2.0, 3.0), list(attack_rate = 2.5)), matrix(c(5.0, 7.5, 5.0, 7.5), nrow = 2, byrow = TRUE))
  expect_error(get_funcresp(t1, 3, c(2.0, 3.0), list(attack_rate = 2.5)))
})
