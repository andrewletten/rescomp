test_that("Differentials are as expected for given state/pars", {

  dN <- def_cr_ode(
    State = c(10,1),
    Pars = suppressMessages(spec_rescomp())
    )
  # Expected: (func_form(R = 1, mu = 0.1, Ks = 1, phi = 0, type3 = 1/2) - 0.03)*10
  expect_equal(dN[[1]][1], 0.7)

  dN <- def_cr_ode(
    State = c(10, 10, 1),
    Pars = suppressMessages(
      spec_rescomp(
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

  dN <- def_cr_ode(
    Time = 6,
    State = c(10, 1),
    Pars = suppressMessages(
      spec_rescomp(
        spnum = 1,
        resnum = 1,
        timepars = TRUE,
        timeparfreq = 10,
        mumatrix = list(matrix(c(0.7), nrow = 1, ncol = 1), matrix(c(1), nrow = 1, ncol = 1))
      )
    )
  )
  # Expected dN1: (func_form(R = 1, mu = 0.7, Ks = 1, phi = 0, type3 = 1/2) - 0.03)*10
  expect_equal(dN[[1]][1], 6.7)


  dN <- def_cr_ode(
    Time = 17,
    State = c(10, 1),
    Pars = suppressMessages(
      spec_rescomp(
        spnum = 1,
        resnum = 1,
        timepars = TRUE,
        timeparfreq = 10,
        mumatrix = list(matrix(c(0.7), nrow = 1, ncol = 1), matrix(c(1), nrow = 1, ncol = 1))
      )
    )
  )
  # Expected dN1: (func_form(R = 1, mu = 1, Ks = 1, phi = 0, type3 = 1/2) - 0.03)*10
  expect_equal(dN[[1]][1], 9.7)

  dN <- def_cr_ode(
    State = c(10, 1, 1),
    Pars = suppressMessages(
      spec_rescomp(
        spnum = 1,
        resnum = 2,
        essential = TRUE,
        mumatrix = list(matrix(c(0.7, 0.3), nrow = 1, ncol = 2))
      )
    )
  )
  # Expected dN1: (func_form(R = 1, mu = 0.3, Ks = 1, phi = 0, type3 = 1/2) - 0.03)*10
  expect_equal(dN[[1]][1], 2.7)

  dN <- def_cr_ode(
    State = c(10, 1, 1),
    Pars = suppressMessages(
      spec_rescomp(
        spnum = 1,
        resnum = 2,
        essential = TRUE,
        chemo = FALSE,
        mumatrix = list(matrix(c(0.7, 0.3), nrow = 1, ncol = 2))
      )
    )
  )
  # Expected dN1: (func_form(R = 1, mu = 0.3, Ks = 1, phi = 0, type3 = 1/2) - 0.03)*10
  expect_equal(dN[[1]][1], 2.7)

  dN <- def_cr_ode(
    State = c(10, 1, 1),
    Pars = suppressMessages(
      spec_rescomp(
        spnum = 1,
        resnum = 2,
        essential = FALSE,
        chemo = FALSE,
        mumatrix = list(matrix(c(0.7, 0.3), nrow = 1, ncol = 2))
      )
    )
  )
  # Expected dN1:
  # (func_form(R = 1, mu = 0.7, Ks = 1, phi = 0, type3 = 1/2))*10 +
  # (func_form(R = 1, mu = 0.3, Ks = 1, phi = 0, type3 = 1/2))*10 - 0.03*10
  expect_equal(dN[[1]][1], 9.7)

  dN <- def_cr_ode(
    Time = 1,
    State = c(10, 1),
    Pars = suppressMessages(
      spec_rescomp(
        spnum = 1,
        resnum = 1,
        funcresp = "type2",
        timepars = TRUE,
        timeparfreq = 10,
        kmatrix = list(matrix(c(0.5), nrow = 1, ncol = 1), matrix(c(0.05), nrow = 1, ncol = 1))
      )
    )
  )
  # Expected dN1: (func_form(R = 1, mu = 0.1, Ks = 0.5, phi = 1, type3 = 1/2) - 0.03)*10
  expect_equal(round(dN[[1]][1], 5), round(0.3666667, 5))

  dN <- def_cr_ode(
    Time = 15,
    State = c(10, 1),
    Pars = suppressMessages(
      spec_rescomp(
        spnum = 1,
        resnum = 1,
        funcresp = "type2",
        timepars = TRUE,
        timeparfreq = 10,
        kmatrix = list(matrix(c(0.5), nrow = 1, ncol = 1), matrix(c(0.05), nrow = 1, ncol = 1))
      )
    )
  )
  # Expected dN1: (func_form(R = 1, mu = 0.1, Ks = 0.05, phi = 1, type3 = 1/2) - 0.03)*10
  expect_equal(round(dN[[1]][1], 5), round(0.652381, 5))

  dN <- def_cr_ode(
    Time = 1,
    State = c(10, 1),
    Pars = suppressMessages(
      spec_rescomp(
        spnum = 1,
        resnum = 1,
        funcresp = "type2",
        timepars = TRUE,
        timeparfreq = 10,
        qmatrix = list(matrix(c(0.5), nrow = 1, ncol = 1),
                       matrix(c(0.05), nrow = 1, ncol = 1))
      )
    )
  )
  # Expected dN2: -(func_form(R = 1, mu = 0.1, Ks = 1, phi = 1, type3 = 1/2))*10*0.5
  expect_equal(dN[[1]][2], -0.25)

  dN <- def_cr_ode(
    Time = 15,
    State = c(10, 1),
    Pars = suppressMessages(
      spec_rescomp(
        spnum = 1,
        resnum = 1,
        funcresp = "type2",
        timepars = TRUE,
        timeparfreq = 10,
        qmatrix = list(matrix(c(0.5), nrow = 1, ncol = 1),
                       matrix(c(0.05), nrow = 1, ncol = 1))
      )
    )
  )
  # Expected dN2: -(func_form(R = 1, mu = 0.1, Ks = 1, phi = 1, type3 = 1/2))*10*0.05
  expect_equal(dN[[1]][2], -0.025)

  dN <- def_cr_ode(
    Time = 1,
    State = c(10, 1),
    Pars = suppressMessages(
      spec_rescomp(
        spnum = 1,
        resnum = 1,
        funcresp = "type2",
        timepars = TRUE,
        timeparfreq = 10,
        mort = list(0.01, 1)
      )
    )
  )
  # Expected dN2: (func_form(R = 1, mu = 0.1, Ks = 1, phi = 1, type3 = 1/2) - 0.01)*10
  expect_equal(dN[[1]][1], 0.4)

  dN <- def_cr_ode(
    Time = 15,
    State = c(10, 1),
    Pars = suppressMessages(
      spec_rescomp(
        spnum = 1,
        resnum = 1,
        funcresp = "type2",
        timepars = TRUE,
        timeparfreq = 10,
        mort = list(0.01, 1)
      )
    )
  )
  # Expected dN2: (func_form(R = 1, mu = 0.1, Ks = 1, phi = 1, type3 = 1/2) - 1)*10
  expect_equal(dN[[1]][1], -9.5)

})

test_that("event funcs gives correct output",{

  NR <- eventfun_respulse(
    State = c(10,1),
    Pars = suppressMessages(spec_rescomp())
    )
  expect_equal(NR, c(10,1))

  NR <- eventfun_respulse(
    State = c(10,1),
    Pars = suppressMessages(spec_rescomp(respulse = 1))
  )
  expect_equal(NR, c(10,2))

  NR <- eventfun_respulse(
    State = c(10,1),
    Pars = suppressMessages(spec_rescomp(mortpulse = 0.2))
  )
  expect_equal(NR, c(8,1))

  NR <- eventfun_respulse(
    State = c(10,1),
    Pars = suppressMessages(spec_rescomp(
      mortpulse = 0.2,
      respulse = 1))
  )
  expect_equal(NR, c(8,2))

  NR <- eventfun_respulse(
    State = c(10,1),
    Pars = suppressMessages(spec_rescomp(
      mortpulse = 0.2,
      respulse = 1,
      batchtrans = TRUE))
  )
  expect_equal(NR, c(8,1))

  NR <- eventfun_starttime(
    Time = 1,
    State = c(0,1),
    Pars = suppressMessages(spec_rescomp(
      introseq = c(10),
      cinit = 10
    ))
  )
  expect_equal(NR, c(0,1))

  NR <- eventfun_starttime(
    Time = 10,
    State = c(0,1),
    Pars = suppressMessages(spec_rescomp(
      introseq = c(10),
      cinit = 10
    ))
  )
  expect_equal(NR, c(10,1))

})
