
test_that("`fisher_power()` generates correct values", {
  expect_equal(
    withr::with_seed(1, fisher_power(0.6, 0.7, 50, 50)),  # low
    0.12
  )
  expect_equal(
    withr::with_seed(1, fisher_power(0.8, 0.7, 100, 100)),  # mid
    0.265
  )
  expect_equal(
    withr::with_seed(1, fisher_power(0.8, 0.7, 350, 350)),  # high
    0.84
  )
})
