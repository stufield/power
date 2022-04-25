#' Calculate Power Table
#'
#' Calculate power and sample size tables for a range of known
#' powers or sample sizes, and relate them to a sensitivity/specificity.
#' The calculation relies on a conversion from sens/spec to KS-distance and
#' an approximation from KS-distance to effect size: `qnorm((ks + 1) / 2) * 2`.
#' From there it is similar to [plotPowerCurves()].
#'
#' @param power Which level(s) of power to evaluate the required samples size.
#' @param n.vec Which sample sizes per group to evaluate the corresponding power.
#' @param bonferroni Integer. The Bonferroni correction to apply.
#' @return A list of:
#' \item{n}{A data frame of the number of samples required in each
#'   comparison group to detect an effect corresponding to a given set of
#'   sens/spec values (60/60 to 90/90) and given vector of power values
#'   (default `power = seq(0.6, 0.95, 0.05)`).
#'   The corresponding KS-distances of the sens/spec values are also included.}
#' \item{power}{A data frame containing
#'   the power to detect an effect corresponding to a given set of sens/spec
#'   values (60/60 to 90/90) given vector of sample sizes in each comparison
#'   group (default `n.vec=seq(20, 100, 10)`). The corresponding KS-distances
#'   of the sens/spec values are also included.}
#' @author Stu Field
#' @seealso [stats::power.t.test()], [ks2delta()]
#' @examples
#' table <- createPowerTable()   # defaults
#' lapply(table, head)
#' @importFrom stats power.t.test
#' @export
createPowerTable <- function(power = seq(0.6, 0.95, 0.05),
                             n.vec = seq(20, 100, 10), bonferroni = 1129) {

  ss        <- seq(0.6, 0.9, 0.01)           # sens/spec vector
  ss_names  <- sprintf("%s/%s", ss * 100, ss * 100)
  ks_v      <- c(ss + ss) - 1    # vector of ks distances corresponding to ss
  delta_vec <- ks2delta(ks_v)    # vector: effect sizes corresponding to ks dist
  ret       <- list()
  ret$n <- sapply(power, function(p)
                  sapply(delta_vec, function(x)
                         power.t.test(n = NULL, delta = x, power = p,
                                     sig.level = 0.05 / bonferroni)$n)) |>
           data.frame()
  names(ret$n) <- sprintf("power=%0.2f", power)
  row.names(ret$n) <- ss_names
  ret$power <- sapply(n.vec, function(s)
                      sapply(delta_vec, function(x)
                             power.t.test(n = s, delta = x, power = NULL,
                                          sig.level = 0.05 / bonferroni)$power)) |>
               data.frame() |> setNames(sprintf("n=%i", n.vec))
  row.names(ret$power) <- ss_names
  ret$n     <- round(ret$n, 0L)
  ret$power <- round(ret$power, 3L)

  ret <- lapply(ret, function(.x) {
    cbind(ks_dist = ks_v, .x)
  })
  structure(
    ret, class = c("power_table", class(ret))
  )
}


#' S3 Plot Method for class "power_table"
#'
#' Plot power curves of call to [createPowerTable()],
#' an object of class "power.table". You can plot either power curves or
#' required sample size curves. This differs from standard power curves
#' in that the independent variable (usually effect size) is a
#' varying sensitivity/specificity value.
#'
#' @rdname createPowerTable
#' @param x An object of class `power_table`, ths result of a call to
#' [createPowerTable()], which a named list (length=2) with "n" and "power".
#' @param plot.power Logical. Should the `power` data frame be plotted?
#' If `FALSE`, the sample sizes are plotted.
#' @param file Character. Optional file path to save plot.
#' @param ... Additional arguments passed to [graphics::matplot()]
#' @seealso [graphics::matplot()]
#' @examples
#' # S3 plot method
#' plot(table)
#' plot(table, plot.power = FALSE)
#'
#' @importFrom graphics matplot abline grid axis box
#' @export
plot.power_table <- function(x, plot.power = TRUE, file = NULL, ...) {
  withr::local_par(list(par.def))
  figure(file, scale = 1.1, width = 12, height = 9)
  if ( !is.null(file) ) withr::defer(dev.off())

  if ( plot.power ) {
    x <- x$power
  } else {
    x <- x$n
  }

  x <- x[, names(x) != "ksDist" ]

  if ( plot.power ) {
    y_lab <- bquote(Power~(1-beta))
    title <- "Sample Size"
  } else {
    y_lab <- "Sample Size (per group)"
    title <- expression(1 - beta)
  }

  matplot(x, type = "n", axes = FALSE,
          main = sprintf("%s vs. Sensitivity/Specificity",
                         ifelse(plot.power, "Statistical Power", "Sample Size")),
          xlab = "Sens/Spec", ylab = y_lab)

  grid(nx = 0, ny = NULL, col = "gray60")
  abline(v = seq(1, nrow(x), by = 5), col = "gray60", lty = 3)
  matplot(x, type = "l", pch = 20, axes = FALSE, add = TRUE,
          col = col_string, lty = 1, lwd = 1.5, ...)
  axis(1, at = seq(1, nrow(x), by = 5),
       labels = rownames(x)[seq(1, nrow(x), by = 5)])
  axis(2)
  box()
  legend(ifelse(plot.power, "bottomright", "topright"),
         legend = gsub(".*=", "", names(x)), title = title,
         col = col_string, lty = 1, ncol = 2, lwd = 2)
}
