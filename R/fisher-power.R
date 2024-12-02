#' Fisher Exact Empirical Power
#'
#' Calculates the empirical power of a test through simulated data
#'   for a given effect size of sample size (per group).
#'
#' @inheritParams params
#' @inheritParams t_test_power
#'
#' @param p1 The first proportion (group 1).
#' @param p2 The second proportion (group 2).
#' @param n1 The group 1 sample size (column 1).
#' @param n2 The group 2 sample size (column 2).
#'
#' @examples
#' fisher_power(0.8, 0.7, 100, 100)
#'
#' fisher_power(0.6, 0.7, 50, 60)
#' @importFrom stats fisher.test rbinom
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

