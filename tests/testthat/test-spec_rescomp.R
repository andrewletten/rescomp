test_that("Correct model messages", {
  expect_snapshot(spec_rescomp())
  expect_snapshot(spec_rescomp(spnum = 2))
  expect_snapshot(spec_rescomp(resnum = 2))
  expect_snapshot(spec_rescomp(linear = FALSE))
  expect_snapshot(spec_rescomp(essential = TRUE))
  expect_snapshot(spec_rescomp(chemo = TRUE))
  expect_snapshot(spec_rescomp(batchtrans = TRUE))
#  expect_snapshot(spec_rescomp(timepars = TRUE)) throws error which is correct
})


test_that("Correct errors and fixes", {

  # error
  expect_error(suppressMessages(spec_rescomp(timepars = TRUE)))
  # fix
  expect_error(suppressMessages(spec_rescomp(
    timepars = TRUE,
    mumatrix = list(matrix(1), matrix(1)),
    timeparfreq = 100)),
    NA)

  # error
  expect_error(suppressMessages(spec_rescomp(
    linear = TRUE,
    kmatrix = matrix(1))))
  # fix
  expect_error(suppressMessages(spec_rescomp(
    linear = FALSE,
    kmatrix = matrix(1))),
    NA)
  # fix
  expect_error(suppressMessages(spec_rescomp(
    linear = TRUE)),
    NA)

  # error
  expect_error(suppressMessages(spec_rescomp(timepars = TRUE)))
  # fix
  expect_error(suppressMessages(spec_rescomp(
    timepars = TRUE,
    mumatrix = list(matrix(1), matrix(1)),
    timeparfreq = 100)),
    NA)

  # error
  expect_error(suppressMessages(
    spec_rescomp(cinit = c(10,11))))
  # fix
  expect_error(suppressMessages(
    spec_rescomp(cinit = c(10,11), spnum = 2)),
    NA)

})
