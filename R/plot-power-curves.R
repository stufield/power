#' Plot Power Curves
#'
#' Uses [stats::power.t.test()] to generate power
#'   values and plots of multiple power curves.
#'
#' @param delta_vec `numeric(n)`. Sequence of effect sizes.
#' @param power_vec `numeric(n)`. Sequence of powers (lines) to plot.
#' @param alpha `double(1)`. The significance, the probability of a false positive.
#' @param bonferroni `integer(1)`. Bonferroni multiple testing correction.
#' @param main `character(1)`. Optional title.
#'
#' @return A plot.
#' @author Stu Field
#'
#' @seealso [stats::power.t.test()], [graphics::matplot()]
#'
#' @examples
#' plot_power_curves(effect = seq(0.5, 2, 0.1), power_vec = seq(0.5, 0.9, 0.1))

#' @importFrom stats power.t.test
#' @importFrom ggplot2 guides ggplot aes geom_point geom_line
#' @importFrom ggplot2 scale_color_manual labs guide_legend
#' @export
plot_power_curves <- function(delta_vec = c(0.5, 2, 0.1),
                              power_vec = seq(0.5, 0.9, 0.1),
                              alpha = 0.05,
                              bonferroni = 1000, main = NULL) {

  tbl_n <- expand.grid(delta_vec, power_vec) |>
    setNames(c("delta", "power")) |>
    apply(1, function(i) {
      power.t.test(power = i["power"], delta = i["delta"],
                   sig.level = 0.05 / bonferroni)$n
    }) |> matrix(nrow = length(delta_vec),
                 dimnames = list(NULL, sprintf("power=%0.2f", power_vec))) |>
    cbind(delta = delta_vec) |>
    as_tibble()

  if ( is.null(main) ) {
     main <- bquote("Power Curves | t-test | Bonferroni corrected ("~.(bonferroni)~")"~"|"~ alpha == .(alpha))
  }

  cols <- col_string[seq_along(power_vec)]

  tidyr::gather(tbl_n, key = "Power", value = "y", -delta) |>
    ggplot(aes(x = delta, y = y, color = Power)) +
    geom_point(alpha = 0.7, size = 2, shape = 19) +
    geom_line() +
    scale_color_manual(values = unname(cols)) +
    labs(x = "Effect Size (delta)",
         y = "Samples / Group (n)",
         title = main) +
    guides(color = guide_legend(title = bquote(beta)))
}
