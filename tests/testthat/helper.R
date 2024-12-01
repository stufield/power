
# fisher_curve <- fisher_power_curve(seq(50, 400, 2), nsim = 250L)
# t_curve <- t_power_curve(seq(10, 100, 2), delta = 0.5, nsim = 250L,
#                          verbose = FALSE)
# saveRDS(fisher_curve, file = test_path("testdata/fisher-curve.rds"))
# saveRDS(t_curve, file = test_path("testdata/t-curve.rds"))

fisher_curve <- readRDS(test_path("testdata/fisher-curve.rds"))
t_curve <- readRDS(test_path("testdata/t-curve.rds"))
