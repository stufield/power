#' Calculate Empirical t-test Power
#'
#' Calculates the empirical power of a test through simulated data
#'   for a given effect size of sample size (per group).
#'
#' @param n `integer(1)`. The sample size.
#' @param delta `numeric(1)`. The effect size.
#' @param nsim `integer(1)`. Number of simulations to perform
#'   in estimating \eqn{1 - \beta}.
#' @param alpha `double(1)`. The significance threshold (\eqn{\alpha}).
#'
#' @return The value of the empirically calculated power (\eqn{1 - \beta}).
#'
#' @author Stu Field
#' @seealso [t.test()], [qt()], [rnorm()]
#'
#' @examples
#' t_test_power(10, 0.5, 25)
#' @importFrom stats t.test qt rnorm
#' @export
t_test_power <- function(n, delta, nsim, alpha = 0.05) {

  y          <- rep(0:1, each = n)
  sims       <- replicate(nsim, c(rnorm(n, 0, 1), rnorm(n, delta, 1)))
  which_cont <- which(y == 0)
  t_vec      <- apply(sims, 2, function(.t)
                      t.test(.t[which_cont], .t[-which_cont])$statistic)
  t_crit     <- qt(alpha, df = 2 * n - 2, lower.tail = FALSE)
  typeII     <- sum(abs(t_vec) < t_crit) / nsim

  if ( typeII > 1 ) {
    stop("Bad type II error calculation: beta > 1", call. = FALSE)
  }
  1 - typeII   # beta (power)
}
