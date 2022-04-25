#' Plot Power Curves
#'
#' Uses [stats::power.t.test()] to generate power
#' values and plots of multiple power curves.
#'
#' @param effect Numeric. Effect sizes as a sequence of values.
#' @param power.vec Numeric. Vector of power to plot.
#' @param alpha Numeric. Alpha for the text, the probability of a false positive.
#' @param bonferroni Integer. Bonferroni multiple testing correction
#' for the number of tests performed.
#' @param main Character. An optional title for the main plot.
#' @return A plot of power curves and the data matrix (of sample sizes) used
#' to create the plot is also returned (invisibly), with the power values
#' as the columns and effect sizes as the rows.
#' @author Stu Field
#' @seealso [stats::power.t.test()], [graphics::matplot()]
#' @examples
#' pwr <- plotPowerCurves(effect    = seq(0.5, 2, 0.1),
#'                        power.vec = seq(0.5, 0.9, 0.1))
#' pwr
#' @importFrom stats power.t.test
#' @importFrom graphics axis matplot grid legend
#' @export
plotPowerCurves <- function(effect = c(0.5, 2, 0.1),
                            power.vec = seq(0.5, 0.9, 0.1),
                            alpha = 0.05,
                            bonferroni = 1129, main = NULL) {

  mat <- sapply(power.vec, function(.b)
                sapply(effect, function(.x)
                       power.t.test(delta = .x, power = .b,
                                    sig.level = alpha / bonferroni)$n))
  dimnames(mat) <- list(sprintf("%0.2f", effect), sprintf("%0.2f", power.vec))

  if ( is.null(main) ) {
     main <- bquote("t-test Power Curves | Bonferroni corrected ("~ n == .(bonferroni)~"|"~ alpha == .(alpha))
  }

  withr::local_par(list(mar = c(4, 5, 3, 1)))
  matplot(mat, type = "l", axes = FALSE, main = main,
          cex.axis = 2, cex.lab = 2, cex.main = 2,
          xlab = "Effect Size (sd)", lwd = 1.5, lty = 1,
          ylab = "Number of Samples / Group (n)")

  grid(col = "gray50")
  matplot(mat, type = "b", lty = 1, pch = 21, bg = "gray", lwd = 1.5,
          axes = FALSE, add = TRUE, col = col_string)
  axis(1, at = seq(length(effect)), labels = effect)
  axis(2)
  box()
  legend("topright", legend = colnames(mat), col = col_string, title = "Power",
         ncol = length(power.vec) %/% length(col_string) + 1,
         lty = 1, lwd = 1.5, cex = 1.5)
  invisible(mat)
}
