test_that("Differentials are as expected for given state/pars", {

  dN <- def_cr_ode(
    State = c(10,1),
    Pars = suppressMessages(make_par_list())
    )
  # Expected: (func_form(R = 1, mu = 0.1, Ks = 1, phi = 0, type3 = 1/2) - 0.03)*10
  expect_equal(dN[[1]][1], 0.7)

  dN <- def_cr_ode(
    State = c(10, 10, 1),
    Pars = suppressMessages(
      make_par_list(
        spnum = 2,
        resnum = 1,
        mumatrix = list(matrix(c(0.7,1), nrow = 2, ncol = 1))
        )
      )
    )
  # Expected dN1: (func_form(R = 1, mu = 0.7, Ks = 1, phi = 0, type3 = 1/2) - 0.03)*10
  # Expected dN2: (func_form(R = 1, mu = 1, Ks = 1, phi = 0, type3 = 1/2) - 0.03)*10
  expect_equal(dN[[1]][1], 6.7)
  expect_equal(dN[[1]][2], 9.7)

})
