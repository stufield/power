
# Setup ----
n <- withr::with_seed(1,
  t_power_curve(seq(10, 50, 2), delta = 0.66, nsim = 25L)
)

delta <- withr::with_seed(2,
  t_power_curve(seq(0.5, 2.5, 0.2), n = 20, nsim = 25L)
)


# Testing ----
test_that("`t_power_curve()` returns expected values and structure; n", {
  expect_s3_class(n, "t_power_curve")
  expect_s3_class(n, "class_n")
  expect_named(n, c("tbl", "constant.label", "constant", "label",
                    "variable", "sequence", "reps", "nsim"))
  expect_equal(n$label, "Sample")
  expect_equal(n$variable, "n")
  expect_equal(n$nsim, 25)
  expect_equal(n$constant.label, "delta")
  expect_equal(n$constant, 0.66)
  expect_equal(n$nsim, 25)
  expect_equal(names(n$tbl), as.character(seq(10, 50, 2)))
  expect_equal(dim(n$tbl), c(25, 21))
  expect_equal(n$sequence, seq(10, 50, 2))
  # values
  expect_equal(sum(n$tbl), 404.44)
  # S3 print
  expect_snapshot(n)
})

test_that("`t_power_curve()` returns expected values and structure; delta", {
  expect_s3_class(delta, "t_power_curve")
  expect_s3_class(delta, "class_delta")
  expect_named(delta, c("tbl", "constant.label", "constant", "label",
                        "variable", "sequence", "reps", "nsim"))
  expect_equal(delta$label, "Effect")
  expect_equal(delta$variable, "delta")
  expect_equal(delta$nsim, 25)
  expect_equal(delta$nsim, 25)
  expect_equal(delta$constant.label, "n")
  expect_equal(delta$constant, 20)
  expect_equal(names(delta$tbl), as.character(seq(0.5, 2.5, 0.2)))
  expect_equal(dim(delta$tbl), c(25, 11))
  expect_equal(delta$sequence, seq(0.5, 2.5, 0.2))
  # values
  expect_equal(sum(delta$tbl), 250.16)
  # S3 print
  expect_snapshot(delta)
})
