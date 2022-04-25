#' Wrapper to Create Power Simulation Data
#'
#' Wrapper to simplify creating data from [simulatePowerData()].
#'
#' @param par.list List. A named list of length 2, each containing a
#' vector of values corresponding to the desired values to evaluate each of
#' `n` and `delta`. Default values correspond to a typical range of
#' "n" and "delta" observed in clinical data.
#' @param delta Default effect size while evaluating `n`.
#' @param n Default sample size per group while evaluating `delta`.
#' @param ... Arguments passed to [simulatePowerData()], typically
#' `nboot`, which *must* be passed.
#' @return A list of length 2, each the result of a call to
#' [simulatePowerData()], and containing both the "sample size"
#' simulation and the "effect size" simulation.
#' @author Stu Field
#' @seealso [simulatePowerData()], [calcEmpPower()]
#' @examples
#' sims <- powerWrapper(nboot = 25, verbose = TRUE)
#' lapply(sims, head)
#' @export
powerWrapper <- function(par.list = list(n     = seq(2, 30, 2),
                                         delta = seq(0.5, 2.5, 0.1)),
                          delta = 1.2, n = 20, ...) {
  stopifnot(length(par.list) == 2)
  ret <- list()
  cat("* Sample sizes:\n")
  ret$n <- simulatePowerData(par.list$n, delta = delta, ...)
  cat("* Effect sizes:\n")
  ret$delta <- simulatePowerData(par.list$delta, n = n, ...)
  ret
}
