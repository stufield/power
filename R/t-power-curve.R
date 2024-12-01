#' Create Power Curve for t-tests
#'
#' Create a power curve from simulated data for either
#'   a given set of sample size or effect size values (`variable`),
#'   while holding sample or effect size (whichever is not
#'   defined in `variable`) constant.
#'
#' @inheritParams t_test_power
#'
#' @param sequence `numeric(n)`. A sequence of values to vary the
#'   appropriate variable, either `n` or `delta`.
#' @param reps `integer(1)`. Number of simulations *per box* to generate,
#'   i.e. number of points within each simulation box.
#' @param n `integer(1)`. The value for the number of samples
#'   per group to hold constant.
#' @param delta `double(1)`. The value for the effect size to hold constant.
#' @param verbose `logical(1)`. Should function be run in verbose mode?
#' @param ... Additional arguments passed either to [t_test_power()],
#'   only `alpha =`, or to the S3 plotting method.
#'
#' @return A `t_power_curve` class object.
#' @seealso [fisher_power_curve()]
#'
#' @author Stu Field
#'
#' @examples
#' # constant effect size
#' size <- t_power_curve(seq(10, 50, 2), delta = 0.66, nsim = 25)
#' size
#'
#' # constant sample size
#' delta <- t_power_curve(seq(0.5, 2.5, 0.2), n = 10, nsim = 25)
#' delta
#' @importFrom helpr signal_info add_class set_Names
#' @export
t_power_curve <- function(sequence, n = NULL, delta = NULL, nsim = 200L,
                          reps = 25L, verbose = interactive(), ...) {

  if ( missing(sequence) ) {
    stop("The `sequence =` argument is missing:
         You must provide a sequence of values for `delta` or `n`
         e.g. `seq(0.2, 2.5, 0.1)`",
         call. = FALSE)
  }

  if ( is.null(n) && is.null(delta) ) {
    stop("Both n and delta cannot be `NULL`", call. = FALSE)
  }

  if ( !(is.null(n) || is.null(delta)) ) {
    stop("Cannot pass *both* `n` AND `delta`.", call. = FALSE)
  }

  type <- ifelse(is.null(n), "n", "delta")
  ret  <- list()

  ret$tbl <- set_Names(sequence) |>
    lapply(function(.v) {
      if ( verbose ) {
        signal_info(
          "Simulating:",
          sprintf(ifelse(type == "n", "%02i", "%0.1f"), .v)
        )
      }
      vapply(seq(reps), function(.x) {
             # simulating over n (fixed delta)
             if ( type == "n" ) {
               t_test_power(n = .v, delta = delta, nsim = nsim, ...)
             # simulating over delta (fixed n)
             } else if ( type == "delta" ) {
               t_test_power(n = n, delta = .v, nsim = nsim, ...)
             }
      }, double(1))
    }) |> as_tibble()

  ret$constant.label <- ifelse(type == "n", "delta", "n")
  ret$constant <- ifelse(type == "n", delta, n)
  ret$label    <- ifelse(type == "n", "Sample", "Effect")
  ret$variable <- type
  ret$sequence <- sequence
  ret$reps     <- reps
  ret$nsim     <- nsim
  add_class(ret, c("t_power_curve", paste0("class_", type)))
}


#' Print Power Curve Object
#'
#' S3 print method for `t_power_curve` objects
#'
#' @rdname t_power_curve
#'
#' @importFrom helpr symbl signal_rule value add_style pad
#' @export
print.t_power_curve <- function(x, ...) {
  signal_rule("t-test Power Curve Simulation", line_col = "blue")
  left <- c("Sim table",
            "Sims per calculation",
            "Repeats per sim (per box)",
            "Constant",
            "Varying",
            "Sequence") |> pad(width = 25)
  right <- c(paste(dim(x$tbl), collapse = " x "),
             x$nsim,
             x$reps,
             paste0(x$constant.label, " = ", x$constant),
             x$variable,
             value(x$sequence))
  writeLines(paste(add_style$red(symbl$bullet), left, right))
  signal_rule(line_col= "green", lty = "double")
  invisible(x)
}

#' Plot Power Curve Object
#'
#' S3 plot method for "t_power_curve" objects.
#'
#' @rdname t_power_curve
#'
#' @param x A `t_power_curve` class object, the result of a call
#'   to [t_power_curve()].
#'
#' @return A ggplot of boxplots.
#'
#' @examples
#' plot(size)
#'
#' plot(delta)
#' @importFrom graphics boxplot
#' @importFrom ggplot2 alpha geom_boxplot labs geom_hline
#' @export
plot.t_power_curve <- function(x, ...) {
  const <- ifelse(x$variable == "delta", "n", bquote(delta))
  xlev  <- x$sequence
  title <- bquote(.(x$label) ~ "Size vs Power |"~italic(t)-test~"|"~ n[sim] == .(x$nsim) ~"|"~ n[reps] == .(x$nsim) ~"|"~ .(const) == .(x$constant))
  xlab  <- switch(x$variable, delta = bquote(Effect~size~(delta)),
                  n = "Sample Size (per group)")

  x$tbl |>
    tidyr::gather(key = "x", value = "y") |>
    ggplot(aes(x = factor(x, levels = xlev), y = y)) +
    geom_boxplot(alpha = 0.5, notch = FALSE, fill = col_string["purple"]) +
    labs(y = bquote(Power~(1 - beta)), x = xlab) +
    ggtitle(title) +
    geom_hline(yintercept = c(0.75, 0.85), linetype = "dashed",
               color = col_string["lightblue"])
}
