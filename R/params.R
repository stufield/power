#' Common Parameters in \pkg{power}
#'
#' The parameters below are commonly used throughout
#'   the \pkg{power} package.
#'
#' @name params
#'
#' @param nsim `integer(1)`. Number of simulations to perform
#'   in estimating power, (\eqn{1 - \beta}).
#'
#' @param alpha `double(1)`. The significance threshold (\eqn{\alpha}),
#'   the probability of a significant result given the null hypothesis,
#'   i.e. a false positive. If desired, Bonferroni correction should
#'   be implemented here.
#'
#' @param n `integer(1)`. The sample size.
#'
#' @param n_vec `integer(n)`. A sequence of sample sizes per group
#'   to evaluate the corresponding power.
#'
#' @param delta `double(1)`. The effect size.
#'
#' @param delta_vec `numeric(n)`. A sequence of effect sizes
#'   to evaluate the corresponding power.
#'
#' @param power_vec `numeric(n)` in \verb{(0, 1)}. A sequence of
#'   power levels to evaluate the required samples size.
#'
#' @param main `character(1)`. Optional title.
NULL
