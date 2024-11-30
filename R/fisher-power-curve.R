#' Create a Power Curve for Fisher's Exact
#'
#' Generates data for a power curve where power is empirically
#'   estimated over a range of sample size values.
#'
#' @inheritParams fisher_power
#'
#' @param n `integer(n)`. A vector of sample sizes per group.
#' @param p `double(1)`. Binomial probability of group 1 (the base group).
#' @param p_diff `double(1)` in \verb{[-1, 1]}. The difference from `p`
#'   of group 2. Can be negative.
#' @param ... Passed to [fisher_power()].
#' @examples
#' tbl <- fisher_power_curve(seq(50, 400, 10), nsim = 100L)
#' tbl
#'
#' @seealso [t_power_curve()]
#' @importFrom helpr add_class
#' @export
fisher_power_curve <- function(n, p = 0.85, p_diff = -0.1, nsim = 200L, ...) {
  tibble::tibble(
    n     = n,
    power = vapply(n, function(.x) {
      fisher_power(p1 = p, p2 = p + p_diff, n1 = .x, n2 = .x, nsim = nsim, ...)
    }, 0.1)
  ) |>
    add_class("fisher_power_curve") |>
    structure(nsim = nsim, p = p, p_diff = p_diff)
}

#' Print Power Curve Object
#'
#' S3 print method for `fisher_power_curve` objects.
#'
#' @rdname fisher_power_curve
#'
#' @param x A `fisher_power_curve` class object.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth ggtitle labs
#' @importFrom helpr symbl signal_rule value add_style pad
#' @export
print.fisher_power_curve <- function(x, ...) {
  signal_rule("Fisher's Exact Power Curve Simulation", line_col = "blue")
  left <- c("Sim table",
            "Sims per calculation",
            "p", "delta",
            "Varying",
            "Sequence `n`") |> pad(width = 25)
  right <- c(paste(dim(x), collapse = " x "),
             attr(x, "nsim"),
             attr(x, "p"),
             attr(x, "p_diff"),
             "n",
             value(x$n))
  writeLines(paste(add_style$red(symbl$bullet), left, right))
  signal_rule(line_col= "green", lty = "double")
  invisible(x)
}


#' Plot Power Curve Object
#'
#' S3 plot method for `fisher_power_curve` objects.
#'
#' @rdname fisher_power_curve
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth ggtitle labs
#' @examples
#' plot(tbl)
#' @export
plot.fisher_power_curve <- function(x, ...) {
  p     <- attr(x, "p")
  delta <- attr(x, "p_diff")
  nsim  <- attr(x, "nsim")
  title <- bquote("Fisher's Exact Power Curve |" ~ n[sim] == .(nsim) ~"|"~ rho == .(p) ~"|"~ delta == .(delta))

  x |>
    ggplot(aes(x = n, y = power)) +
    geom_point(alpha = 0.7, col = "#24135F") +
    geom_smooth(formula = y ~ x, method = "loess") +
    labs(x = "Counts per Group", y = bquote(Power~(1-beta))) +
    ggtitle(title)
}


