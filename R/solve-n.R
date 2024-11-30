#' Solve for `n` at specific power value.
#'
#' Solve for the sample size (`n`) for
#'   a specified power value. This function
#'   effectively inverts the power curve and
#'   efficiently returns the sample size
#'   for a given power.
#'
#' @param x A "power_curve" object. See [fisher_power_curve()]
#'   or [t_power_curve()].
#' @param pwr `double(1)` in \verb{(0, 1)}.
#'   The power to be interpolated.
#' @examples
#' tbl <- fisher_power_curve(seq(50, 400, 25))
#' solve_n(tbl, 0.85)
#'
#' solve_n(tbl, 0.75)
#'
#' solve_n(tbl, 0.55)
#' @export
solve_n <- function(x, pwr) {
  UseMethod("solve_n")
}

#' @noRd
#' @importFrom helpr value
#' @export
solve_n.default <- function(x, pwr) {
  stop(
    "Could not determine S3 `solve_n()` method for class: ",
    value(class(x)),
    call. = FALSE
  )
}

# This is the primary method
# A tibble with `n` and `power` columns
#' @noRd
#' @importFrom stats loess optimize predict
#' @export
solve_n.tbl_df <- function(x, pwr) {
  fit <- loess(power ~ n, data = x)
  min_pwr <- min(fit$fitted)
  max_pwr <- max(fit$fitted)
  if ( pwr < min_pwr || pwr > max_pwr ) {
    stop("`pwr` is outside of interpolated range: ", pwr, call. = FALSE)
  }
  fn <- function(x) {  # objective function
    y <- unname(predict(fit, data.frame(n = x)))
    alpha <- (y - pwr) / (1 - pwr)  # proportion of range above pwr
    # alpha < 0 if y is below pwr; return y
    # alpha > 0; threshold y and decrease by proportion above pwr (alpha)
    # creates a peaked objective function at pwr to maximize below
    ifelse(alpha > 0, pwr - (pwr * alpha), y)
  }
  #approx(x = fit$fitted, y = x$n, xout = pwr) |> unlist()
  range <- range(as.numeric(fit$x))
  opt <- optimize(fn, interval = range, maximum = TRUE, tol = 0.001)
  round(c(power = opt$objective, n = opt$maximum), 3L)
}

#' @noRd
#' @export
solve_n.fisher_power_curve <- function(x, pwr) {
  stopifnot(
    "Sanity check ... `n` and `power` must be in the tibble!" =
      all(c("n", "power") %in% names(x))
  )
  NextMethod()
}

#' @noRd
#' @export
solve_n.t_power_curve <- function(x, pwr) {
  # Stop to ensure you're not acting on a `class_delta` object
  stopifnot(
    "Power curve object must vary `n` to solve for it." = inherits(x, "class_n")
  )
  NextMethod()
}

#' @noRd
#' @importFrom tibble enframe
#' @importFrom stats median
#' @export
solve_n.class_n <- function(x, pwr) {
  tbl <- vapply(x$tbl, stats::median, double(1)) |>
    enframe(name = "n", value = "power")
  solve_n(tbl, pwr = pwr)
}
