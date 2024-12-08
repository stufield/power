#' Convert KS Distance to Effect Size (Delta)
#'
#' Convert KS-distance to effect size using the standard
#'   inverse error function conversion: `qnorm( (x + 1) / 2) * 2`.
#'
#' @rdname ks
#'
#' @param x `numeric(n)` in \verb{[0, 1]}. A sequences of KS-distance(s).
#'   Or a `ks_pwr_table` object, from call to [ks_power_table()],
#'   which is a `tibble` with `n` and `power`.
#'
#' @return Effect sizes corresponding to `x`.
#' @author Stu Field
#' @seealso [qnorm()]
#'
#' @examples
#' ks2delta(0.45)
#'
#' ks2delta(seq(0.1, 0.9, length.out = 5))
#' @importFrom stats qnorm
#' @export
ks2delta <- function(x) {
  stopifnot("`x` must be in [0, 1]." = x > 0 & x < 1)
  qnorm((x + 1) / 2) * 2
}


#' Calculate KS Power Table
#'
#' Calculate power and sample size tables for a range of known
#'   powers or sample sizes, and relate them to a sensitivity/specificity.
#'   The calculation relies on a conversion from sens/spec to KS-distance and
#'   an approximation from KS-distance to effect size: `qnorm((ks + 1) / 2) * 2`.
#'
#' @rdname ks
#'
#' @inheritParams params
#'
#' @return A list of:
#'   \item{n}{A data frame of the number of samples required in each
#'     comparison group to detect an effect corresponding to a given set of
#'     sens/spec values (60/60 to 90/90) and given vector of power values
#'     (default `power_vec = seq(0.6, 0.95, 0.05)`).
#'     The corresponding KS-distances of the sens/spec values are also included.}
#'   \item{power}{A data frame containing
#'     the power to detect an effect corresponding to a given set of sens/spec
#'     values (60/60 to 90/90) given vector of sample sizes in each comparison
#'     group (default `n_vec = seq(20, 100, 10)`). The corresponding KS-distances
#'     of the sens/spec values are also included.
#'   }
#' @author Stu Field
#'
#' @seealso [stats::power.t.test()], [ks2delta()]
#'
#' @examples
#' tables <- ks_power_table()
#' tables
#' @importFrom helpr set_Names
#' @importFrom stats power.t.test
#' @importFrom tibble as_tibble enframe
#' @export
ks_power_table <- function(power_vec = seq(0.6, 0.95, 0.05),
                           n_vec = seq(20, 100, 10),
                           alpha = 0.05) {

  ss        <- seq(0.6, 0.9, by = 0.01) # sens/spec vector
  ss_names  <- sprintf("%s/%s", ss * 100, ss * 100)
  ks_v      <- c(ss + ss) - 1    # vector of ks distances corresponding to ss
  delta_vec <- ks2delta(ks_v)    # vector: effect sizes corresponding to ks dist
  base <- enframe(set_Names(ks_v, ss_names), name = "SS", value = "KS")

  tbl_n <- expand.grid(delta_vec, power_vec) |>
    set_Names(c("delta", "power")) |>
    apply(1, function(i) {
      power.t.test(n = NULL, power = i["power"],
                   delta = i["delta"], sig.level = alpha)$n
    }) |> matrix(nrow = length(delta_vec),
                 dimnames = list(NULL, sprintf("power=%0.2f", power_vec))) |>
    as_tibble()

  tbl_pwr <- expand.grid(delta_vec, n_vec) |>
    set_Names(c("delta", "n")) |>
    apply(1, function(i) {
    power.t.test(power = NULL, n = i["n"], delta = i["delta"],
                 sig.level = alpha)$power
    }) |> matrix(nrow = length(delta_vec),
                 dimnames = list(NULL, sprintf("n=%i", n_vec))) |>
    as_tibble()

  tbl_n <- dplyr::bind_cols(base, tbl_n)
  tbl_pwr <- dplyr::bind_cols(base, tbl_pwr)

  structure(
    list(n = tbl_n, power = tbl_pwr),
    class = c("ks_pwr_table", "list")
  )
}


#' S3 Plot Method for class "ks_pwr_table"
#'
#' Plot power curves of call to [ks_power_table()],
#'   an object of class `ks_pwr_table`. You can plot
#'   either power curves or required sample size curves.
#'   This differs from standard power curves
#'   in that the independent variable (usually effect size)
#'   is a varying sensitivity/specificity value.
#'
#' @rdname ks
#'
#' @param plot_power `logical(1)`. Should the `power` data frame be plotted?
#'   If `FALSE`, the sample sizes are plotted.
#' @param ... Additional arguments as requpred by the [plot()] generic.
#' @examples
#' # S3 plot method
#' plot(tables)
#'
#' plot(tables, plot_power = FALSE)
#' @importFrom ggplot2 ggplot aes geom_line geom_point guides labs
#' @importFrom ggplot2 guide_legend scale_color_manual guide_axis
#' @export
plot.ks_pwr_table <- function(x, plot_power = TRUE, ...) {

  if ( plot_power ) {
    x <- x$power
  } else {
    x <- x$n
  }

  if ( plot_power ) {
    y_lab <- bquote(Power~(1-beta))
    legend_title <- "Sample Size"
  } else {
    y_lab <- "Sample Size (per group)"
    legend_title <- bquote(1-beta)
  }
  main <- sprintf("%s vs. Sensitivity/Specificity",
                 ifelse(plot_power, "Statistical Power", "Sample Size"))
  levs <- grep("^n=|^power=", names(x), value = TRUE)

  tidyr::gather(x, key = "Power", value = "y", -SS, -KS) |>
    ggplot(aes(x = SS, y = y, color = factor(Power, levs),
               group = factor(Power, levs))) +
    geom_point(alpha = 0.7, size = 2, shape = 19) +
    geom_line() +
    scale_color_manual(
      values = unname(col_string),
      labels = function(x) gsub(".*=", "", x)) +
    labs(x = "Sens/Spec", y = y_lab, title = main) +
    guides(color = guide_legend(title = legend_title),
           x = ggplot2::guide_axis(angle = 45))
}
