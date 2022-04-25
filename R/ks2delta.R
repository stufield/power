#' Convert KS Distance to Effect Size (Delta)
#'
#' Convert KS-distance to effect size using the standard
#' inverse error function conversion: `qnorm( (x + 1) / 2) * 2`.
#'
#' @param x A KS-distance value or a vector of KS-distances.
#' @return Effect sizes corresponding to `x`.
#' @author Stu Field
#' @seealso [qnorm()]
#' @examples
#' ks2delta(0.45)
#' @importFrom stats qnorm
#' @export
ks2delta <- function(x) qnorm((x + 1) / 2) * 2
