#' Create Power Simulation Data Frame
#'
#' Create a data frame containing the power for either
#'   a given set of sample size or effect size values (`variable`),
#'   while holding sample or effect size (whichever is not
#'   defined in `variable`) constant.
#'
#' @inheritParams t_test_power
#'
#' @param sequence Sequence of values to vary the
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
#' @return A `power_sim` class object.
#'
#' @author Stu Field
#'
#' @examples
#' # constant effect size
#' size <- simulate_power_t(seq(10, 50, 2), delta = 0.5, nsim = 25)
#' size
#'
#' # constant sample size
#' delta <- simulate_power_t(seq(0.5, 2.5, 0.1), n = 10, nsim = 25)
#' delta
#' @importFrom stats setNames
#' @export
simulate_power_t <- function(sequence, n = NULL, delta = NULL, nsim = 50L,
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

  ret$sim <- lapply(sequence, function(.v) {
    if ( verbose ) {
      cat("* simulating: ")
    }
    vapply(seq(reps), function(.x) {
           if ( verbose ) {
             if ( .x == 1 ) {
               cat(sprintf(ifelse(type == "n", "%02i .", "%0.1f ."), .v))
             } else if ( .x == reps ) {
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
  }) |> data.frame() |> setNames(sequence)

  ret$constant.label <- ifelse(type == "n", "delta", "n")
  ret$constant <- ifelse(type == "n", delta, n)
  ret$label    <- ifelse(type == "n", "Sample", "Effect")
  ret$variable <- type
  ret$sequence <- sequence
  ret$reps     <- reps
  ret$nsim     <- nsim
  structure(ret, class = c("power_sim", class(ret)))
}


#' Plot Power Simulation Data
#'
#' S3 print method for "power_sim" objects.
#'
#' @rdname simulate_power_t
#'
#' @param x An object of class `power_sim`, the result of a call
#'   to [simulate_power_t()].
#' @param file Character. Optional file name to save file if desired.
#'   Default is `NULL`, the default graphics device.
#'
#' @return A plot of power simulations.
#' @examples
#' plot(size)
#'
#' plot(delta)
#' @importFrom graphics boxplot
#' @importFrom ggplot2 alpha
#' @export
plot.power_sim <- function(x, ..., file = NULL) {
  withr::local_options(list(warn = -1))
  withr::local_par(par_def)
  boxplot(x$sim, col = alpha("blue", 0.75), notch = TRUE, cex.lab = 2,
          ylab = bquote(Power~(1 - beta)), ylim = 0:1,
          xlab = if ( x$variable == "n") "Sample Size (per group)" else bquote(Effect~size~(delta)),
          outpch = 21, outbg = "red", cex = 0.75, ...)
  const <- ifelse(x$variable == "n", bquote(delta), "n")
  bquote(.(x$label) ~ "size vs Power |" ~ n[sim] == .(x$nsim) ~"|"~ n[reps] == .(x$nsim) ~"|"~ .(const) == .(x$constant)) |>
    title(cex.main = 1.5)
  add_box(0.75, 0.85, col = "darkgreen", alpha = 0.2)
  abline(h = c(0.75, 0.85), lty = 2, col = "red")
}

#' Print Power Simulation Object
#'
#' S3 print method for `power_sim` objects
#'
#' @rdname simulate_power_t
#' @export
print.power_sim <- function(x, ...) {
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
