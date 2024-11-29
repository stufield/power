
col_string <- c("dodgerblue", "red", "darkgreen",
                "darkorchid4", "cyan", "orange",
                "black", "grey", "#990066", "green", "#24135F")

par_def <- list(mgp = c(2.00, 0.75, 0.00), mar = c(3, 4, 3, 1))

#' @importFrom graphics par rect
#' @noRd
addBox <- function(bottom = NULL, top = NULL, left = NULL, right = NULL,
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

#' @importFrom grDevices pdf png postscript jpeg dev.off
#' @noRd
figure <- function(file, height = 9, width = 9, scale = 1, res = 300, ...) {
  if (is.null(file)) {
    return(invisible(NULL))
  }
  if (grepl("\\.pdf$", file)) {
    pdf(file = file, height = height * scale, width = width * scale,
        useDingbats = FALSE, title = sub("\\.pdf$", "", basename(file)), ...)
  }
  else if (grepl("\\.png$", file)) {
    png(filename = file, height = height * scale, width = width * scale,
        units = "in", res = res, ...)
  }
  else if (grepl("\\.eps$", file)) {
    postscript(file = file, height = height * scale * 100,
               width = width * scale * 100, horizontal = FALSE,
               onefile = FALSE, paper = "special", ...)
  }
  else if (grepl("\\.jpeg$", file)) {
    jpeg(filename = file, height = height * 500, width = width * 500, res = 600, ...)
  }
  else {
    stop("Could not find file extension in provided file path: ", file, call. = FALSE)
  }
}
