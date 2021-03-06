test_that("Unique levels in output match arguments to spec_rescomp", {

  pars1 <- suppressMessages(spec_rescomp())
  df1 <- df_funcresp(pars1)
  expect_equal(length(unique(df1$sp)), pars1$nconsumers)
  expect_equal(length(unique(df1$paramstate)), 1)
  expect_equal(length(unique(df1$resource)), pars1$nresources)

  pars2 <- suppressMessages(spec_rescomp(spnum = 2))
  df2 <- df_funcresp(pars2)
  expect_equal(length(unique(df2$sp)), pars2$nconsumers)
  expect_equal(length(unique(df2$paramstate)), 1)
  expect_equal(length(unique(df2$resource)), pars2$nresources)

  pars3 <- suppressMessages(spec_rescomp(spnum = 2, resnum = 2))
  df3 <- df_funcresp(pars3)
  expect_equal(length(unique(df3$sp)), pars3$nconsumers)
  expect_equal(length(unique(df3$paramstate)), 1)
  expect_equal(length(unique(df3$resource)), pars3$nresources)

  pars4 <- suppressMessages(
    spec_rescomp(
      spnum = 2, resnum = 2,
      timepars = TRUE,
      totaltime = 1000,
      timeparfreq = 10,
      mumatrix = list(matrix(c(1, 1,
                               1, 1),
                             nrow = 2, ncol = 2),
                      matrix(c(2, 2,
                               2, 2),
                             nrow = 2, ncol = 2))))
  df4 <- df_funcresp(pars4)
  expect_equal(length(unique(df4$sp)), pars3$nconsumers)
  expect_equal(length(unique(df4$paramstate)), 2)
  expect_equal(length(unique(df4$resource)), pars4$nresources)

})

test_that("Resource range argument works as expected", {

  pars <- suppressMessages(spec_rescomp())
  df <- df_funcresp(pars, maxx = 2)
  expect_equal(max(df$resource.levels), 2)

  pars <- suppressMessages(spec_rescomp())
  df <- df_funcresp(pars)
  expect_equal(max(df$resource.levels), 1)

  pars <- suppressMessages(spec_rescomp())
  expect_error(df_funcresp(pars, maxx = -1))

  pars <- suppressMessages(spec_rescomp())
  expect_error(df_funcresp(pars, maxx = "1"))

})

