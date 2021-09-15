test_that("Correct model messages", {
  expect_snapshot(spec_rescomp())
  expect_snapshot(spec_rescomp(spnum = 2))
  expect_snapshot(spec_rescomp(resnum = 2))
  expect_snapshot(spec_rescomp(funcresp = "type2"))
  expect_snapshot(spec_rescomp(essential = TRUE))
  expect_snapshot(spec_rescomp(chemo = TRUE))
  expect_snapshot(spec_rescomp(chemo = TRUE,
                               respulse = 1))
  expect_snapshot(spec_rescomp(chemo = TRUE,
                               resspeed = 0,
                               respulse = 1))
  expect_snapshot(spec_rescomp(chemo = FALSE,
                               resspeed = 0,
                               respulse = 1))
  expect_snapshot(spec_rescomp(chemo = FALSE,
                               resspeed = 0,
                               respulse = 0))
  expect_snapshot(spec_rescomp(chemo = FALSE,
                               resspeed = 1,
                               respulse = 0))
  expect_snapshot(spec_rescomp(chemo = TRUE,
                               resspeed = 0,
                               respulse = 0))
  expect_snapshot(spec_rescomp(chemo = TRUE,
                               resspeed = 1,
                               respulse = 0))
  expect_snapshot(spec_rescomp(chemo = FALSE,
                               resspeed = 1,
                               respulse = 1))
  expect_snapshot(spec_rescomp(batchtrans = TRUE))
  expect_snapshot(spec_rescomp(mortpulse = 0.5))
  expect_snapshot(spec_rescomp(mortpulse = 0.5,
                               respulse = 1))
  expect_snapshot(spec_rescomp(mortpulse = 0.5,
                               mort = 0))
  expect_snapshot(spec_rescomp(mort = 0))
  expect_snapshot(spec_rescomp(timepars = TRUE,
                               mumatrix = list(matrix(1), matrix(1)),
                               timeparfreq = 100,
                               tpinterp = "inst"))
  expect_snapshot(spec_rescomp(timepars = TRUE,
                               mumatrix = list(matrix(1), matrix(1)),
                               timeparfreq = 100,
                               tpinterp = "lin"))
  expect_snapshot(spec_rescomp(timepars = TRUE,
                               mumatrix = list(matrix(1), matrix(1)),
                               timeparfreq = 100,
                               tpinterp = "sine"))
  expect_snapshot(spec_rescomp(funcresp = "type3"))
  expect_snapshot(spec_rescomp(spnum = 2,
                               funcresp = c("type1", "type3")))


})


test_that("Correct errors and fixes", {

  # error
  expect_error(suppressMessages(spec_rescomp(
    timepars = TRUE)),
    "only one mu-, k-, q-matrix and mortality vector")
  expect_error(suppressMessages(spec_rescomp(
    timepars = TRUE,
    mumatrix = list(matrix(1), matrix(1)))),
    "timeparfreq must be provided")
  expect_error(suppressMessages(spec_rescomp(
    timepars = FALSE,
    mumatrix = list(matrix(1), matrix(1)))),
    "more than one mu, k, and/or q matrix")
  expect_error(suppressMessages(spec_rescomp(
    timepars = TRUE,
    timeparfreq = 100)),
    "only one mu-, k-, q-matrix and mortality vector")
  # fix
  expect_error(suppressMessages(spec_rescomp(
    timepars = TRUE,
    mumatrix = list(matrix(1), matrix(1)),
    timeparfreq = 100)),
    NA)

  # error
  expect_error(suppressMessages(spec_rescomp(
    spnum = 2,
    resnum = 2,
    mumatrix = list(matrix(c(1,1), nrow = 1, ncol = 2)))),
    "mumatrix\\(s\\) should have")
  expect_error(suppressMessages(spec_rescomp(
    spnum = 2,
    resnum = 2,
    mumatrix = list(matrix(c(1,1), nrow = 2, ncol = 1)))),
    "mumatrix\\(s\\) should have")
  # fix
  expect_error(suppressMessages(spec_rescomp(
    spnum = 2,
    resnum = 2,
    mumatrix = list(matrix(c(1,1,1,1), nrow = 2, ncol = 2)))),
    NA)

  # error
  expect_error(suppressMessages(spec_rescomp(
    funcresp = "type1",
    kmatrix = matrix(1))),
    "Matrix of half saturation constants")
  # fix
  expect_error(suppressMessages(spec_rescomp(
    funcresp = "type2",
    kmatrix = matrix(1))),
    NA)
  # fix
  expect_error(suppressMessages(spec_rescomp(
    funcresp = "type1")),
    NA)

  # error
  expect_error(suppressMessages(
    spec_rescomp(cinit = c(10,11))),
    "Length of cinit must equal spnum")
  # fix
  expect_error(suppressMessages(
    spec_rescomp(cinit = c(10,11), spnum = 2)),
    NA)

  # error
  expect_error(suppressMessages(
    spec_rescomp(qmatrix = 1)))
  # fix
  expect_error(suppressMessages(
    spec_rescomp(qmatrix = matrix(1))),
    NA)

  # error
  expect_error(suppressMessages(
    spec_rescomp(spnum = 2,
                 mort = c(0.1, 0.1, 0.1))))
  # fix
  expect_error(suppressMessages(
    spec_rescomp(spnum = 2,
                 mort = c(0.1, 0.1))),
    NA)

  # error
  expect_error(suppressMessages(
    spec_rescomp(spnum = 2,
                 mort = list(c(0.1, 0.1, 0.1), c(0.1, 0.1, 0.1)))))
  # fix
  expect_error(suppressMessages(
    spec_rescomp(spnum = 2,
                 mort = list(c(0.1, 0.1), c(0.1, 0.1)))),
    NA)

  # error
  expect_error(suppressMessages(
    spec_rescomp(spnum = 1,
                 qmatrix = matrix(1),
                 effmatrix = matrix(1))))
  # fix
  expect_error(suppressMessages(
    spec_rescomp(spnum = 1,
                 effmatrix = matrix(1))),
    NA)

  # error
  expect_error(suppressMessages(
    spec_rescomp(spnum = 1,
                 qmatrix = matrix(1),
                 effmatrix = (1))))
  # fix
  expect_error(suppressMessages(
    spec_rescomp(spnum = 1,
                 effmatrix = matrix(1))),
    NA)

  # error
  expect_error(suppressMessages(
    spec_rescomp(spnum = 1,
                 introseq = c(1, 2))))
  # fix
  expect_error(suppressMessages(
    spec_rescomp(spnum = 1,
                 introseq = c(1))),
    NA)

  # error
  expect_error(suppressMessages(
    spec_rescomp(spnum = 3,
                 funcresp = c("type1", "type2"))))
  # fix
  expect_error(suppressMessages(
    spec_rescomp(spnum = 3,
                 funcresp = c("type1", "type2", "type3"))),
    NA)

  # error
  expect_warning(suppressMessages(
    spec_rescomp(spnum = 2,
                 funcresp = c("type1", "type2"),
                 kmatrix = matrix(c(0.5, 1,
                                    nrow = 2,
                                    byrow = TRUE)))))
  # fix
  expect_warning(suppressMessages(
    spec_rescomp(spnum = 2,
                 funcresp = c("type1", "type2"),
                 kmatrix = matrix(c(1, 1,
                                    nrow = 2,
                                    byrow = TRUE)))),
    NA)


  # error
  expect_warning(suppressMessages(
    spec_rescomp(spnum = 2,
                 funcresp = c("type1", "type2"),
                 timepars = TRUE,
                 timeparfreq = 10,
                 kmatrix = list(matrix(c(0.5, 1,
                                    nrow = 2,
                                    byrow = TRUE)),
                                matrix(c(0.5, 1,
                                         nrow = 2,
                                         byrow = TRUE))))))
  # fix
  expect_warning(suppressMessages(
    spec_rescomp(spnum = 2,
                 funcresp = c("type1", "type2"),
                 timepars = TRUE,
                 timeparfreq = 10,
                 kmatrix = list(matrix(c(1, 1,
                                         nrow = 2,
                                         byrow = TRUE)),
                                matrix(c(1, 1,
                                         nrow = 2,
                                         byrow = TRUE))))),
    NA)

})

test_that("Correct output params", {

   out <- suppressMessages(
    spec_rescomp(
      spnum = 2,
      mort = list(c(0.1), c(0.1))))

   expect_equal(out$all_d, list(c(0.1, 0.1), c(0.1, 0.1)))

   out <- suppressMessages(
     spec_rescomp(
       spnum = 1,
       funcresp = "type2",
       timepars = TRUE,
       timeparfreq = 10,
       kmatrix = list(1, 1)))

   expect_equal(is.function(out$Ks_approx_fun[[1]]), TRUE)

   out <- suppressMessages(
     spec_rescomp(
       spnum = 1,
       funcresp = "type2",
       timepars = TRUE,
       timeparfreq = 10,
       qmatrix = list(1, 1)))

   expect_equal(is.function(out$Qs_approx_fun[[1]]), TRUE)

   out <- suppressMessages(
     spec_rescomp(
       spnum = 1,
       funcresp = "type2",
       timepars = TRUE,
       timeparfreq = 10,
       mort = list(0.1, 0.1)))

   expect_equal(is.function(out$mort_approx_fun[[1]]), TRUE)

})

