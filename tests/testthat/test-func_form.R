test_that("second derivs match type 1, 2 and 3", {

  #################################################
  # Check type 1 2nd div always 0
  x <- seq(0, 1, length.out = 1000)
  f1 <- func_form(R = seq(0, 1, length.out = 1000),
                  mu = 1,
                  Ks = 1,
                  phi = 0,
                  type3 = 1/2)

  # find the average x between 2 points
  avex <- x[-1] - diff(x)/2
  # find the numerical approximation of 1st derivative
  # delta-y/delta-x
  dydx <- diff(f1)/diff(x)
  # 2nd derivative
  d2ydx2 <- diff(dydx)/diff(x[-1])
  expect_true(all(d2ydx2 == 0))


  #################################################
  # Check type 2 2nd div always negative
  x <- seq(0, 1, length.out = 1000)
  f1 <- func_form(R = seq(0, 1, length.out = 1000),
                  mu = 1,
                  Ks = 0.1,
                  phi = 1,
                  type3 = 1/2)

  # find the average x between 2 points
  avex <- x[-1] - diff(x)/2
  # find the numerical approximation of 1st derivative
  # delta-y/delta-x
  dydx <- diff(f1)/diff(x)
  # 2nd derivative
  d2ydx2 <- diff(dydx)/diff(x[-1])
  expect_true(all(d2ydx2 < 0))

  #################################################
  # Check type 2 2nd div positive and negative negative
  x <- seq(0, 1, length.out = 1000)
  f1 <- func_form(R = seq(0, 1, length.out = 1000),
                  mu = 1,
                  Ks = 0.1,
                  phi = 1,
                  type3 = 1)

  # find the average x between 2 points
  avex <- x[-1] - diff(x)/2
  # find the numerical approximation of 1st derivative
  # delta-y/delta-x
  dydx <- diff(f1)/diff(x)
  # 2nd derivative
  d2ydx2 <- diff(dydx)/diff(x[-1])
  expect_false(all(d2ydx2 > 0))
  expect_false(all(d2ydx2 < 0))

})
