#' t-test Empirical Power
#'
#' Calculates the empirical power of a test through simulated data
#'   for a given effect size of sample size (per group).
#'
#' @inheritParams params
#'
#' @param ... Passed to the underlying test, i.e. [fisher.test()],
#'   [t.test()], etc.,  e.g. `alternative =`.
#'
#' @return The value of the empirically calculated power (\eqn{1 - \beta}).
#'
#' @author Stu Field
#' @seealso [t.test()], [qt()], [rnorm()]
#'
#' @examples
#' t_test_power(10, 0.5)
#' @importFrom stats t.test qt rnorm
#' @export
t_test_power <- function(n, delta, nsim = 200L, alpha = 0.05, ...) {

  y    <- rep(0:1L, each = n)
  sims <- replicate(nsim, c(rnorm(n, 0, 1), rnorm(n, delta, 1)))
  idx  <- which(y == 0L)
  t_vec <- apply(sims, 2, function(.t) {
                 t.test(.t[idx], .t[-idx], ...)$statistic
                 })
  t_crit <- qt(alpha, df = 2 * n - 2, lower.tail = FALSE)
  typeII <- sum(abs(t_vec) < t_crit) / nsim

  if ( typeII > 1 ) {
    stop("Bad type II error calculation: beta > 1", call. = FALSE)
  }
  1 - typeII   # beta (power)
}
