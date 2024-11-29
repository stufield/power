
col_string <- c(
  purple     = "#24135F",
  lightgreen = "#00A499",
  lightgrey  = "#707372",
  magenta    = "#840B55",
  lightblue  = "#006BA6",
  yellow     = "#D69A2D",
  darkgreen  = "#007A53",
  darkblue   = "#1B365D",
  darkgrey   = "#54585A",
  blue       = "#004C97"
)


par_def <- list(mgp = c(2.00, 0.75, 0.00), mar = c(3, 4, 3, 1))

#' @importFrom graphics par rect
#' @noRd
add_box <- function(bottom = NULL, top = NULL, left = NULL, right = NULL,
                   col, alpha = 0.2, ...) {
  if (is.null(bottom))
    bottom <- par("usr")[3L]
  if (is.null(left))
    left <- par("usr")[1L]
  if (is.null(top))
    top <- par("usr")[4L]
  if (is.null(right))
    right <- par("usr")[2L]
  rect(left, bottom, right, top, border = NA, col = ggplot2::alpha(col, alpha))
}

