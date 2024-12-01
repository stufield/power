
test_that("`t_test_power()` generates expected output", {
  expect_equal(
    withr::with_seed(1, t_test_power(10, 0.2)), # low
    0.115
  )
  expect_equal(
    withr::with_seed(1, t_test_power(20, 0.5)), # mid
    0.455
  )
  expect_equal(
    withr::with_seed(1, t_test_power(30, 0.8)), # high
    0.93
  )
})
