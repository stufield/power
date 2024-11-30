#' Create a Power Curve for Fisher's Exact
#'
#' Generates data for a power curve where power is empirically
#'   estimated over a range of sample size values.
#'
#' @param n `integer(n)`. A vector of sample sizes per group.
#' @param p `double(1)`. Binomial probability of group 1 (the base group).
#' @param p_diff `double(1)` in \verb{[-1, 1]}. The difference from `p`
#'   of group 2. Can be negative.
#' @param ... Passed to [fisher_power()], e.g. `nsim`.
#' @examples
#' tbl <- fisher_power_curve(seq(50, 400, 25))
#' tbl
#'
#' @seealso [t_power_curve()]
#' @importFrom helpr add_class
#' @export
fisher_power_curve <- function(n, p = 0.85, p_diff = -0.1, ...) {
  tibble::tibble(
    n     = n,
    power = vapply(n, function(.x) {
      fisher_power(p1 = p, p2 = p + p_diff, n1 = .x, n2 = .x, ...)
    }, 0.1)
  ) |>
  add_class("fisher_power_curve")
}

#' @rdname fisher_power_curve
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth ggtitle labs
#' @export
plot.fisher_power_curve <- function(x, ...) {
  x |>
    ggplot(aes(x = n, y = power)) +
    geom_point(alpha = 0.7, col = "#24135F") +
    geom_smooth(formula = y ~ x, method = "loess") +
    ggtitle("Fisher's Exact Power Curve") +
    labs(x = "Counts per Group", y = bquote(Power~(1-beta)))
}

#gg +
#  annotate("segment",
#    x        = c(pwr_n[["n"]], min(power_tbl$n)),
#    xend     = c(pwr_n[["n"]],pwr_n[["n"]]),
#    y        = c(min(power_tbl$power), pwr_n[["power"]]),
#    yend     = c(pwr_n[["power"]], pwr_n[["power"]]),
#    linetype = "dashed", colour = "#00A499")

