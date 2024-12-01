
# Setup ----
# testing objects are retrieved in helper.R

# Testing ----
test_that("`solve_n()` solves for the correct sample size for t-tests", {
  expect_error(
    solve_n(t_curve, 0.1),
    "`pwr` is outside of the interpolated range: 0.1"
  )
  expect_equal(solve_n(t_curve, 0.3), c(power = 0.3, n = 10.55))
  expect_equal(solve_n(t_curve, 0.5), c(power = 0.5, n = 22.535))
  expect_equal(solve_n(t_curve, 0.7), c(power = 0.7, n = 38.525))
  expect_equal(solve_n(t_curve, 0.9), c(power = 0.9, n = 68.768))
})

test_that("`solve_n()` solves for the correct sample size for Fisher's Exact", {
  expect_error(
    solve_n(fisher_curve, 0.1),
    "`pwr` is outside of the interpolated range: 0.1"
  )
  expect_equal(solve_n(fisher_curve, 0.3), c(power = 0.3, n = 84.394))
  expect_equal(solve_n(fisher_curve, 0.5), c(power = 0.5, n = 143.096))
  expect_equal(solve_n(fisher_curve, 0.7), c(power = 0.7, n = 215.928))
  expect_equal(solve_n(fisher_curve, 0.9), c(power = 0.9, n = 347.372))
})

test_that("`solve_n()` error conditions", {
  x <- t_curve
  class(x)[2L] <- "class_delta"   # cannot perform `solve_n()` on delta class
  expect_error(
    solve_n(x, 0.8),
    "Power curve object must vary `n` to solve for it."
  )
  x <- fisher_curve
  names(x) <- c("x", "y")
  expect_error(
    solve_n(x, 0.8),
    "Sanity check ... `n` and `power` must be in the tibble!"
  )
  expect_error(
    solve_n(data.frame(x = 1), 0.5),   # default method
    "Could not determine S3 `solve_n()` method for class: 'data.frame'",
    fixed = TRUE
  )
})
