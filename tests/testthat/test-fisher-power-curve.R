
# Setup ----
sim <- 50L
nvec <- seq(50, 400, 20)
tbl <- withr::with_seed(101,
  fisher_power_curve(nvec, p = 0.85, nsim = sim)
)

# Testing ----
test_that("`fisher_power_curve()` function generates expected output", {
  expect_s3_class(tbl, "fisher_power_curve")
  expect_s3_class(tbl, "tbl_df")
  expect_equal(dim(tbl), c(18L, 2L))
  expect_equal(attr(tbl, "nsim"), sim)
  expect_equal(attr(tbl, "p"), 0.85)
  expect_equal(attr(tbl, "p_diff"), -0.1)
  expect_equal(tbl$n, nvec)
  expect_equal(sum(tbl$power), 11.96)
  # S3 print
  expect_snapshot(tbl)
})
