#' Display a palette as swatches
#'
#' @param x A palette name or a character vector of colours.
#' @param n Passed to [get_pal()] when `x` is a palette name.
#' @param labels Print hex codes on the swatches?
#'
#' @examples
#' print_pal("wilsons_snipe")
#' print_pal("grosbeak_teal_div", n = 11)
#' @export
print_pal <- function(x, n = NULL, labels = TRUE) {
  title <- ""
  if (length(x) == 1 && !grepl("^#", x)) {
    title <- x
    x <- get_pal(x, n = if (is.null(n) && x %in% names(boreal_continuous)) 9 else n)
  }
  k <- length(x)
  old <- graphics::par(mar = c(0.5, 0.5, if (nzchar(title)) 2 else 0.5, 0.5))
  on.exit(graphics::par(old))
  graphics::plot(NULL, xlim = c(0, k), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "", main = title)
  graphics::rect(seq_len(k) - 1, 0, seq_len(k), 1, col = x, border = NA)
  if (labels && k <= 12) {
    lum <- colSums(grDevices::col2rgb(x) * c(0.299, 0.587, 0.114)) / 255
    graphics::text(seq_len(k) - 0.5, 0.5, toupper(x), col = ifelse(lum > 0.55, "black", "white"),
                   cex = if (k > 8) 0.6 else 0.8)
  }
  invisible(x)
}
