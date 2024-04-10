test_that("get_coefs() and associated functions work on a bare vector", {
  n <- 12
  obj <- 0.1 * 1:n

  expect_no_error(check_coefs_vector(obj))
  expect_error(check_coefs_matrix(obj))

  expect_no_error(check_coefs_vector(obj, n))
  expect_error(check_coefs_vector(obj, n + 1))

  expect_no_error(check_coefs(obj, n))
  expect_error(check_coefs(obj, n + 1))
  expect_error(check_coefs(obj, c(n, 1)))
  expect_error(check_coefs(obj, c(1, n)))

  expect_equal(get_coefs_dim(obj), n)
  expect_equal(get_coefs_length(obj), n)
  expect_error(get_coefs_nrow(obj))
  expect_error(get_coefs_ncol(obj))

  expect_equal(get_coefs(obj, NULL), obj)
  expect_equal(get_coefs_vector(obj, NULL), obj)

  expect_error(get_coefs_matrix(obj, NULL))
})

test_that("get_coefs() and associated functions work on a bare matrix", {
  n <- 5
  m <- 6
  obj <- 0.1 * matrix(1:(n * m), nrow = n, ncol = m)

  expect_error(check_coefs_vector(obj))
  expect_no_error(check_coefs_matrix(obj))

  expect_no_error(check_coefs_matrix(obj, n, m))
  expect_no_error(check_coefs_matrix(obj, n, NULL))
  expect_no_error(check_coefs_matrix(obj, NULL, m))
  expect_error(check_coefs_matrix(obj, n + 1, NULL))
  expect_error(check_coefs_matrix(obj, NULL, m + 1))
  expect_error(check_coefs_matrix(obj, n + 1, m))
  expect_error(check_coefs_matrix(obj, n, m + 1))
  expect_error(check_coefs_matrix(obj, n + 1, m + 1))

  expect_no_error(check_coefs(obj, c(n, m)))
  expect_error(check_coefs(obj, c(n + 1, m)))
  expect_error(check_coefs(obj, c(n, m + 1)))
  expect_error(check_coefs(obj, n))

  expect_equal(get_coefs_dim(obj), c(n, m))
  expect_error(get_coefs_length(obj))
  expect_equal(get_coefs_nrow(obj), n)
  expect_equal(get_coefs_ncol(obj), m)

  expect_equal(get_coefs(obj, NULL), obj)
  expect_equal(get_coefs_matrix(obj, NULL), obj)

  expect_error(get_coefs_vector(obj, NULL))
})

test_that("check_coefs() catches errors", {
  expect_no_error(check_coefs(1:3, 3, "example"))
  expect_no_error(check_coefs(matrix(1:12, nrow = 3, ncol = 4), c(3, 4), "example"))

  expect_error(check_coefs(1:3, c(), "example"))
  expect_error(check_coefs(1:3, c(3, 3), "example"))
  expect_error(check_coefs(1:3, 4, "example"))
  expect_error(check_coefs(matrix(1:12, nrow = 3, ncol = 4), c(4, 3), "example", c("spnum", "resnum")))
  expect_error(check_coefs(c("a", "b", "c"), 3, "example"))
  expect_error(check_coefs(list(1, 2, 3), 3, "example"))
  expect_error(check_coefs(data.frame(1:3, 2:4, 3:5), c(3, 3), "example"))

  expect_error(check_coefs(array(1:60, dim = c(3, 4, 5)), c(3, 4, 5), "example"))
})

test_that("check_coefs_coordinate() catches errors", {
  expect_no_error(check_coefs_coordinate(1:3, 2:4))
  expect_no_error(check_coefs_coordinate(1:3, 2:4, check_dims = 1))
  expect_no_error(check_coefs_coordinate(matrix(1:12, nrow = 3, ncol = 4), matrix(2:13, nrow = 3, ncol = 4)))
  expect_no_error(check_coefs_coordinate(matrix(1:12, nrow = 3, ncol = 4), matrix(2:13, nrow = 3, ncol = 4), check_dims = 1))
  expect_no_error(check_coefs_coordinate(matrix(1:12, nrow = 3, ncol = 4), matrix(2:13, nrow = 3, ncol = 4), check_dims = 2))
  expect_no_error(check_coefs_coordinate(matrix(1:12, nrow = 3, ncol = 4), matrix(2:13, nrow = 3, ncol = 4), check_dims = c(1, 2)))
  expect_no_error(check_coefs_coordinate(matrix(1:12, nrow = 3, ncol = 4), matrix(1:9, nrow = 3, ncol = 3), check_dims = 1))
  expect_no_error(check_coefs_coordinate(matrix(1:12, nrow = 3, ncol = 4), matrix(1:16, nrow = 4, ncol = 4), check_dims = 2))

  expect_error(check_coefs_coordinate(1:3, 2:4, check_dims = 2))
  expect_error(check_coefs_coordinate(1:3, 2:4, check_dims = c(1, 2)))
  expect_error(check_coefs_coordinate(matrix(1:12, nrow = 3, ncol = 4), matrix(2:13, nrow = 3, ncol = 4), check_dims = 3))
  expect_error(check_coefs_coordinate(matrix(1:12, nrow = 3, ncol = 4), matrix(2:13, nrow = 3, ncol = 4), check_dims = c(1, 2, 3)))

  expect_error(check_coefs_coordinate(1:12, matrix(1:12, nrow = 3, ncol = 4)))
  expect_error(check_coefs_coordinate(1:3, matrix(1:12, nrow = 3, ncol = 4)))
  expect_error(check_coefs_coordinate(1:4, matrix(1:12, nrow = 3, ncol = 4)))

  expect_error(check_coefs_coordinate(1:3, 1:4))
  expect_error(check_coefs_coordinate(1:3, 1:4, check_dims = 1))
  expect_error(check_coefs_coordinate(matrix(1:12, nrow = 3, ncol = 4), matrix(1:12, nrow = 4, ncol = 3)))
  expect_error(check_coefs_coordinate(matrix(1:12, nrow = 3, ncol = 4), matrix(1:12, nrow = 4, ncol = 3), check_dims = 1))
  expect_error(check_coefs_coordinate(matrix(1:12, nrow = 3, ncol = 4), matrix(1:12, nrow = 4, ncol = 3), check_dims = 2))
  expect_error(check_coefs_coordinate(matrix(1:12, nrow = 3, ncol = 4), matrix(1:12, nrow = 4, ncol = 3), check_dims = c(1, 2)))
  expect_error(check_coefs_coordinate(matrix(1:12, nrow = 3, ncol = 4), matrix(1:9, nrow = 3, ncol = 3)))
  expect_error(check_coefs_coordinate(matrix(1:12, nrow = 3, ncol = 4), matrix(1:9, nrow = 3, ncol = 3), check_dims = 2))
  expect_error(check_coefs_coordinate(matrix(1:12, nrow = 3, ncol = 4), matrix(1:16, nrow = 4, ncol = 4), check_dims = 1))
  expect_error(check_coefs_coordinate(matrix(1:12, nrow = 3, ncol = 4), matrix(1:9, nrow = 3, ncol = 3), check_dims = c(1, 2)))
})

test_that("rescomp_coefs_custom types work", {
  vec_func <- function(params) {
    c(0.2, 0.3, 0.4, params$fourth_coeff)
  }
  mat_func <- function(params) {
    matrix(c(0.2, 0.3, 0.4, params$fourth_coeff), nrow = 2, ncol = 2)
  }
  params <- list(fourth_coeff = 0.5)

  custom_vec <- rescomp_coefs_vector_custom(vec_func, length = 4)
  expect_equal(get_coefs(custom_vec, params), c(0.2, 0.3, 0.4, 0.5))
  expect_equal(get_coefs_vector(custom_vec, params), c(0.2, 0.3, 0.4, 0.5))
  expect_error(get_coefs_matrix(custom_vec, params))
  expect_equal(get_coefs_dim(custom_vec), 4)
  expect_equal(get_coefs_length(custom_vec), 4)

  custom_mat <- rescomp_coefs_matrix_custom(mat_func, nrow = 2, ncol = 2)
  expect_equal(get_coefs(custom_mat, params), matrix(c(0.2, 0.3, 0.4, 0.5), nrow = 2, ncol = 2))
  expect_equal(get_coefs_matrix(custom_mat, params), matrix(c(0.2, 0.3, 0.4, 0.5), nrow = 2, ncol = 2))
  expect_error(get_coefs_vector(custom_mat, params))
  expect_equal(get_coefs_dim(custom_mat), c(2, 2))
  expect_equal(get_coefs_nrow(custom_mat), 2)
  expect_equal(get_coefs_ncol(custom_mat), 2)

  custom_vec_wrong <- rescomp_coefs_vector_custom(vec_func, length = 5)
  expect_error(get_coefs(custom_vec_wrong, params))
  expect_error(get_coefs_vector(custom_vec_wrong, params))

  custom_mat_wrong <- rescomp_coefs_matrix_custom(mat_func, nrow = 2, ncol = 3)
  expect_error(get_coefs(custom_vec_wrong, params))
  expect_error(get_coefs_matrix(custom_vec_wrong, params))
})

test_that("rescomp_coefs_lerp() works", {
  lerp_vec <- rescomp_coefs_lerp(c(0, 0), c(1, 2), "a")
  expect_equal(get_coefs(lerp_vec, list(a = 0)), c(0, 0))
  expect_equal(get_coefs(lerp_vec, list(a = 1)), c(1, 2))
  expect_equal(get_coefs(lerp_vec, list(a = 0.2)), c(0.2, 0.4))

  lerp_vec_2 <- rescomp_coefs_lerp(c(0, 0), c(1, 2), "a", param0 = 3, param1 = 1)
  expect_equal(get_coefs(lerp_vec_2, list(a = 3)), c(0, 0))
  expect_equal(get_coefs(lerp_vec_2, list(a = 1)), c(1, 2))
  expect_equal(get_coefs(lerp_vec_2, list(a = 2.6)), c(0.2, 0.4))

  lerp_mat <- rescomp_coefs_lerp(matrix(1:4, nrow = 2, ncol = 2), matrix(4:1, nrow = 2, ncol = 2), "a")
  expect_equal(get_coefs(lerp_mat, list(a = 0)), matrix(1:4, nrow = 2, ncol = 2))
  expect_equal(get_coefs(lerp_mat, list(a = 1)), matrix(4:1, nrow = 2, ncol = 2))
  expect_equal(get_coefs(lerp_mat, list(a = 0.5)), matrix(2.5, nrow = 2, ncol = 2))

  lerp_lerp <- rescomp_coefs_lerp(
    rescomp_coefs_lerp(c(0, 0), c(1, 2), "a"),
    rescomp_coefs_lerp(c(1, 2), c(3, 4), "a"),
    "b"
  )
  expect_equal(get_coefs(lerp_lerp, list(a = 0, b = 0)), c(0, 0))
  expect_equal(get_coefs(lerp_lerp, list(a = 1, b = 0)), c(1, 2))
  expect_equal(get_coefs(lerp_lerp, list(a = 0, b = 1)), c(1, 2))
  expect_equal(get_coefs(lerp_lerp, list(a = 1, b = 1)), c(3, 4))
  expect_equal(get_coefs(lerp_lerp, list(a = 0.5, b = 0)), c(0.5, 1))
  expect_equal(get_coefs(lerp_lerp, list(a = 0.5, b = 1)), c(2, 3))
  expect_equal(get_coefs(lerp_lerp, list(a = 0.5, b = 0.5)), c(1.25, 2))

  expect_error(rescomp_coefs_lerp(1:3, 1:4, "a"))
  expect_error(rescomp_coefs_lerp(matrix(1:4, nrow = 2, ncol = 2), matrix(1:6, nrow = 2, ncol = 3), "a"))
  expect_error(rescomp_coefs_lerp(1:4, matrix(1:4, nrow = 2, ncol = 2), "a"))
})
