#' Plot Power Curves
#'
#' Uses [stats::power.t.test()] to generate power
#'   values and plots of multiple power curves.
#'
#' @param effect `numeric(n)`. Sequence of effect sizes.
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
#' @importFrom graphics axis matplot grid legend
#' @export
plot_power_curves <- function(effect = c(0.5, 2, 0.1),
                              power_vec = seq(0.5, 0.9, 0.1),
                              alpha = 0.05,
                              bonferroni = 1000, main = NULL) {

  mat <- sapply(power_vec, function(.b)
                sapply(effect, function(.x)
                       power.t.test(delta = .x, power = .b,
                                    sig.level = alpha / bonferroni)$n))
  dimnames(mat) <- list(sprintf("%0.2f", effect),
                        sprintf("%0.2f", power_vec))

  if ( is.null(main) ) {
     main <- bquote("t-test Power Curves | Bonferroni corrected ("~.(bonferroni)~")"~"|"~ alpha == .(alpha))
  }

  withr::local_par(par_def)
  cols <- col_string[seq_along(power_vec)]
  matplot(mat, type = "n", axes = FALSE, main = main,
          cex.axis = 1, cex.lab = 1, cex.main = 1,
          xlab = "Effect Size (delta)", lwd = 1.5, lty = 1,
          ylab = "Samples / Group (n)")

  grid(col = "gray50")
  matplot(mat, type = "b", lty = 1, pch = 21, bg = "gray", lwd = 1.5,
          axes = FALSE, add = TRUE, col = cols)
  axis(1, at = seq(length(effect)), labels = effect)
  axis(2)
  box()
  legend("topright", legend = colnames(mat), col = cols, title = bquote(beta),
         ncol = length(power_vec) %/% length(col_string) + 1L,
         lty = 1, lwd = 1.5, cex = 1)
  invisible(mat)
}
