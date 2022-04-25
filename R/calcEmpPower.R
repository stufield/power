#' Calculate Empirical Power
#'
#' Simulate the empirical power of a test through simulated data
#' with a given effect size of sample size (per group).
#'
#' @param n Integer. Sample size.
#' @param delta Numeric. Effect size.
#' @param nboot Integer. Number of simulations to perform
#' in estimating `1 - beta`.
#' @param alpha Numeric. The significance threshold.
#' @param bonferroni Numeric. Size of the Bonferroni correction to adjust for.
#' @return The value of the empirically calculated power (`1 - beta`).
#' @author Stu Field
#' @seealso [t.test()], [qt()], [rnorm()]
#' @examples
#' calcEmpPower(10, 15, 1.5)
#' @importFrom stats t.test qt rnorm
#' @export
calcEmpPower <- function(n, delta, nboot, alpha = 0.05, bonferroni = 1129) {

  if ( missing(nboot) ) {
    stop("Must provide `nboot =` ... ", call. = FALSE)
  }

  Response   <- rep(c("neg", "pos"), each = n)
  sims       <- replicate(nboot, c(rnorm(n, 0, 1), rnorm(n, delta, 1)))
  which_cont <- which(Response == "neg")
  t_vec      <- apply(sims, 2, function(.t)
                      t.test(.t[which_cont], .t[-which_cont])$statistic)
  t_crit     <- qt(alpha / bonferroni, df = 2 * n - 2, lower.tail = FALSE)
  typeII     <- sum(abs(t_vec) < t_crit) / nboot

  if ( typeII > 1 ) {
    stop("Bad type II error calculation: beta > 1", call. = FALSE)
  }
  1 - typeII   # return beta (power)
}
