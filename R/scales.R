#' ggplot2 colour and fill scales from boreal bird palettes
#'
#' @param name Palette name. See [list_palettes()].
#' @param discrete Use a discrete scale? Set `FALSE` for continuous data, which
#'   uses [ggplot2::scale_colour_gradientn()].
#' @param reverse Reverse the colour order?
#' @param ... Passed to [ggplot2::discrete_scale()] or
#'   [ggplot2::scale_colour_gradientn()] / [ggplot2::scale_fill_gradientn()].
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'   ggplot(iris, aes(Sepal.Length, Petal.Length, colour = Species)) +
#'     geom_point() +
#'     scale_colour_boreal("blue_winged_teal")
#'
#'   ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'     geom_raster() +
#'     scale_fill_boreal("teal_seq", discrete = FALSE)
#' }
#' @export
scale_colour_boreal <- function(name, discrete = TRUE, reverse = FALSE, ...) {
  boreal_scale("colour", name, discrete, reverse, ...)
}

#' @rdname scale_colour_boreal
#' @export
scale_color_boreal <- scale_colour_boreal

#' @rdname scale_colour_boreal
#' @export
scale_fill_boreal <- function(name, discrete = TRUE, reverse = FALSE, ...) {
  boreal_scale("fill", name, discrete, reverse, ...)
}

boreal_scale <- function(aesthetic, name, discrete, reverse, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is needed for this function.", call. = FALSE)
  }
  lookup_pal(name)  # fail early on a bad name
  if (discrete) {
    ggplot2::discrete_scale(
      aesthetics = aesthetic,
      palette = function(n) get_pal(name, n, type = "discrete", reverse = reverse),
      ...
    )
  } else {
    cols <- get_pal(name, n = 256, type = "continuous", reverse = reverse)
    if (aesthetic == "colour") ggplot2::scale_colour_gradientn(colours = cols, ...)
    else ggplot2::scale_fill_gradientn(colours = cols, ...)
  }
}
