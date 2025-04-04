#' Plot Power Curves
#'
#' Uses [stats::power.t.test()] to generate power
#'   values and plots of multiple power curves.
#'
#' @inheritParams params
#'
#' @return A `ggplot`.
#'
#' @author Stu Field
#'
#' @seealso [stats::power.t.test()]
#'
#' @examples
#' plot_power_curves(delta_vec = seq(0.5, 2, 0.1),
#'                   power_vec = seq(0.5, 0.9, 0.1))
#' @importFrom stats power.t.test
#' @importFrom ggplot2 guides ggplot aes geom_point geom_line
#' @importFrom ggplot2 scale_color_manual labs guide_legend
#' @export
plot_power_curves <- function(delta_vec = c(0.5, 2, 0.1),
                              power_vec = seq(0.5, 0.9, 0.1),
                              alpha = 0.05, main = NULL) {

  tbl_n <- expand.grid(delta_vec, power_vec) |>
    set_Names(c("delta", "power")) |>
    apply(1, function(i) {
      power.t.test(power = i["power"], delta = i["delta"], sig.level = 0.05)$n
    }) |> matrix(nrow = length(delta_vec),
                 dimnames = list(NULL, sprintf("power=%0.2f", power_vec))) |>
    cbind(delta = delta_vec) |>
    as_tibble()

  if ( is.null(main) ) {
     main <- bquote("Power Curves | "~ italic(t) ~"-test | "~ alpha == .(alpha))
  }

  cols <- col_string[seq_along(power_vec)]

  tidyr::gather(tbl_n, key = "Power", value = "y", -delta) |>
    ggplot(aes(x = delta, y = y, color = Power)) +
    geom_point(alpha = 0.7, size = 2, shape = 19) +
    geom_line() +
    scale_color_manual(
      values = unname(cols),
      labels = function(x) gsub(".*=", "", x)) +
    labs(x = "Effect Size (delta)",
         y = "Samples / Group (n)",
         title = main) +
    guides(color = guide_legend(title = bquote(beta)))
}
