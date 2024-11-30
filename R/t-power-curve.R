#' Create Power Simulation Data Frame
#'
#' Create a data frame containing the power for either
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
#' @importFrom stats setNames
#' @export
t_power_curve <- function(sequence, n = NULL, delta = NULL, nsim = 50L,
                          reps = 25L, verbose = interactive(), ...) {

  if ( missing(sequence) ) {
    stop("The `sequence =` argument is missing:
         You must provide a sequence of values for 'delta' or 'n'
         e.g. `seq(0.2, 2.5, 0.1)`",
         call. = FALSE)
  }

  if ( is.null(n) + is.null(delta) == 2 ) {
    stop("Both n and delta cannot be `NULL`", call. = FALSE)
  }

  if ( is.null(n) + is.null(delta) == 0 ) {
    stop("Cannot pass *both* `n` AND `delta`.", call. = FALSE)
  }

  type <- ifelse(is.null(n), "n", "delta")
  ret  <- list()

  ret$sim <- setNames(sequence, sequence) |>
    lapply(function(.v) {
      if ( verbose ) {
        cat("* simulating:",
            sprintf(ifelse(type == "n", "%02i .", "%0.1f ."), .v))
      }
      vapply(seq(reps), function(.x) {
             if ( verbose ) {
               if ( .x == reps ) {
                 cat(".\n")
               } else {
                 cat(".")
               }
             }
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
  structure(ret, class = c("t_power_curve", class(ret)))
}


#' Plot Power Simulation Data
#'
#' S3 print method for "t_power_curve" objects.
#'
#' @rdname t_power_curve
#'
#' @param x An object of class `t_power_curve`, the result of a call
#'   to [t_power_curve()].
#' @param file Character. Optional file name to save file if desired.
#'   Default is `NULL`, the default graphics device.
#'
#' @return A plot of power simulations.
#' @examples
#' plot(size)
#'
#' plot(delta)
#' @importFrom graphics boxplot
#' @importFrom ggplot2 alpha geom_boxplot labs geom_hline
#' @export
plot.t_power_curve <- function(x, ..., file = NULL) {
  const <- ifelse(x$variable == "n", bquote(delta), "n")
  title <- bquote(.(x$label) ~ "size vs Power |" ~ n[sim] == .(x$nsim) ~"|"~ n[reps] == .(x$nsim) ~"|"~ .(const) == .(x$constant))

  x$sim |>
    tidyr::gather(key = "x", value = "y") |>
    ggplot(aes(x = x, y = y)) +
    geom_boxplot(alpha = 0.5, notch = FALSE, fill = col_string["purple"]) +
    labs(y = bquote(Power~(1 - beta)),
         x = if ( x$variable == "n") "Sample Size (per group)" else bquote(Effect~size~(delta))) +
    ggtitle(title) +
    geom_hline(yintercept = c(0.75, 0.85), linetype = "dashed",
               color = col_string["lightblue"])
}

#' Print Power Simulation Object
#'
#' S3 print method for `t_power_curve` objects
#'
#' @rdname t_power_curve
#' @export
print.t_power_curve <- function(x, ...) {
  cat("Power Simulation Info:\n\n")
  left <- c("Simulation table dims",
            "Simulations per Power calculation",
            "Simulating across (varying)",
            "Varying sequence",
            "Constant",
            "Simulation repeats") |> encodeString(width = 35, justify = "left")
  right <- c(paste(dim(x$sim), collapse = " x "),
             x$nsim,
             x$variable,
             paste(x$sequence, collapse = ","),
             paste0(x$constant.label, " = ", x$constant),
             x$nsim)
  writeLines(paste(left, right))
  invisible(x)
}
