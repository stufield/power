#' Solve for `n` at specific power value.
#'
#' Solve for the sample size (`n`) for
#'   a specified power value. This function
#'   effectively inverts the power curve and
#'   efficiently returns the sample size
#'   for a given power.
#'
#' @param pwr_tbl A `pwr_tbl` object.
#' @param pwr `double(1)` in \verb{(0, 1)}.
#'   The power to be interpolated.
#' @importFrom stats loess
#' @examples
#' tbl <- fisher_power_curve(seq(50, 400, 25))
#' solve_n(tbl, 0.85)
#'
#' solve_n(tbl, 0.75)
#'
#' solve_n(tbl, 0.55)
#' @export
solve_n <- function(pwr_tbl, pwr) {
  UseMethod("solve_n")
}

#' @noRd
#' @importFrom helpr value
#' @export
solve_n.default <- function(pwr_tbl, pwr) {
  stop(
    "Could not determine S3 `solve_n()` method for class: ",
    value(class(pwr_tbl)),
    call. = FALSE
  )
}

#' @noRd
#' @export
solve_n.fisher_power_curve <- function(pwr_tbl, pwr) {
  fit <- loess(power ~ n, data = pwr_tbl)
  min_pwr <- min(fit$fitted)
  max_pwr <- max(fit$fitted)
  if ( pwr < min_pwr || pwr > max_pwr ) {
    stop("`pwr` is outside of interpolated range: ", pwr, call. = FALSE)
  }
  # objective function
  fn <- function(x) {
    y <- unname(predict(fit, data.frame(n = x)))
    alpha <- (y - pwr) / (1 - pwr)  # proportion of range above pwr
    # alpha < 0 if y is below pwr (return y)
    # if alpha > 0, threshold y and decrease by proportion above pwr (alpha)
    # creates a peaked objective function at pwr to maximize below
    ifelse(alpha > 0, pwr - (pwr * alpha), y)
  }
  #approx(x = fit$fitted, y = pwr_tbl$n, xout = pwr) |> unlist()
  range <- range(as.numeric(fit$x))
  opt <- optimize(fn, interval = range, maximum = TRUE, tol = 0.001)
  round(c(power = opt$objective, n = opt$maximum), 3L)
}

#' @noRd
#' @export
solve_n.t_power_curve <- function(pwr_tbl, pwr) {
  stopifnot(
    "Power curve object must vary `n` to solve for it." =
      inherits(pwr_tbl, "class_n")
  )
  NextMethod()
}

#' @noRd
#' @export
solve_n.class_n <- function(pwr_tbl, pwr) {
  pwr_tbl <- vapply(pwr_tbl$tbl, stats::median, double(1)) |>
    tibble::enframe(name = "n", value = "power")
  fit <- loess(power ~ n, data = pwr_tbl)
  min_pwr <- min(fit$fitted)
  max_pwr <- max(fit$fitted)
  if ( pwr < min_pwr || pwr > max_pwr ) {
    stop("`pwr` is outside of interpolated range: ", pwr, call. = FALSE)
  }
  fn <- function(x) {
    y <- unname(predict(fit, data.frame(n = x)))
    alpha <- (y - pwr) / (1 - pwr)  # proportion of range above pwr
    ifelse(alpha > 0, pwr - (pwr * alpha), y)
  }
  range <- range(as.numeric(fit$x))
  opt <- optimize(fn, interval = range, maximum = TRUE, tol = 0.001)
  round(c(power = opt$objective, n = opt$maximum), 3L)
}
