#' Fisher Exact Power
#'
#' @param p1 first proportion (group 1)
#' @param p2 second proportion (group 2)
#' @param n1 group 1 sample size (column 1)
#' @param n2 group 2 sample size (column 2)
#' @param nsim number of simulations to perform in calculating power.
#' @param alpha significance level for p-values.
#' @param ... Passed to [fisher.test()], e.g. `alternative`.
#'
#' @export
fisher_power <- function(p1, p2, n1, n2, nsim = 200L, alpha = 0.05, ...) {
  y1 <- rbinom(nsim, n1, p1)
  y2 <- rbinom(nsim, n2, p2)
  m  <- cbind(y1, n1 - y1, y2, n2 - y2)
  mean(
    apply(m, 1, function(.x) {
      fisher.test(matrix(.x, ncol = 2L), ...)$p.value
    }) < alpha
  )
}



#' Fisher Power
#'
#' @param n `integer(n)`. A vector of sample sizes per group.
#' @param p `double(1)`. Binomial probability of group 1 (the base group).
#' @param p_diff `double(1)` in [-1, 1]. The difference from `p`
#'   of group 2. Can be negative.
#' @param ... Passed to [fisher_sim()], e.g. `nsim`.
#' @export
fisher_power_curve <- function(n, p = 0.85, p_diff = -0.1, ...) {
  tibble::tibble(
    n     = n,
    power = vapply(n, function(.x) {
      fisher_power(p1 = p, p2 = p + p_diff, n1 = .x, n2 = .x, ...)
    }, 0.1)
  )
}



plot_fisher <- function(x, ...) {
x |>
  ggplot(aes(x = n, y = power)) +
  geom_point(alpha = 0.7, col = "#24135F") +
  geom_smooth(formula = y ~ x, method = "loess") +
  ggtitle("Fisher's Exact Power Curve") +
  labs(x = "Counts per Group", y = bquote(Power~(1-beta)))
}


#' fit <- loess(power ~ n, data = power_tbl)

solve_n <- function(pwr) {
  fn <- function(n) {   # objective function
    y <- predict(fit, data.frame(n = n))
    ifelse(y <= pwr, y, -y)
  }
  range <- range(as.numeric(fit$x))
  opt <- optimize(fn, interval = range, maximum = TRUE)
  c(power = unname(opt$objective), n = opt$maximum)
}

# approx(x = fit$fitted, y = power_tbl$n, xout = 0.8) |>
#  unlist()

#gg +
#  annotate("segment",
#    x        = c(pwr_n[["n"]], min(power_tbl$n)),
#    xend     = c(pwr_n[["n"]],pwr_n[["n"]]),
#    y        = c(min(power_tbl$power), pwr_n[["power"]]),
#    yend     = c(pwr_n[["power"]], pwr_n[["power"]]),
#    linetype = "dashed", colour = "#00A499")

