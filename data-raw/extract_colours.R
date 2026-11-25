# Pull candidate colours from a plumage photo.
# Not part of the installed package (data-raw/ is in .Rbuildignore).
#
# Workflow per species:
#   1. Put the photo in data-raw/photos/ and record its licence in
#      inst/extdata/photo_credits.csv (own photos, CC0 or CC BY only).
#   2. Crop tightly to the bird (or a patch) so sky, water and vegetation don't
#      swamp the clusters. Cropping several patches separately works well.
#   3. Cluster pixels in CIELAB (more perceptually even than RGB).
#   4. Hand-pick 4-6 clusters, order the most distinct first, paste into R/palettes.R.
#   5. Run data-raw/check_palettes.R.

library(magick)   # install.packages("magick")

extract_lab_clusters <- function(path, k = 10, crop = NULL, max_px = 250, drop_extremes = TRUE) {
  img <- image_read(path)
  if (!is.null(crop)) img <- image_crop(img, crop)   # e.g. "400x300+120+80"
  img <- image_scale(img, paste0(max_px, "x", max_px))
  px <- as.integer(image_data(img, channels = "rgb"))  # h x w x 3, 0-255
  rgb <- matrix(px, ncol = 3) / 255
  if (drop_extremes) {   # drop blown highlights and deep shadow
    lum <- rgb %*% c(0.299, 0.587, 0.114)
    rgb <- rgb[lum > 0.04 & lum < 0.97, , drop = FALSE]
  }
  lab <- grDevices::convertColor(rgb, from = "sRGB", to = "Lab")
  set.seed(1)
  km <- kmeans(lab, centers = k, nstart = 10, iter.max = 50)
  centres <- grDevices::convertColor(km$centers, from = "Lab", to = "sRGB")
  hex <- grDevices::rgb(pmin(pmax(centres, 0), 1))
  out <- data.frame(hex = hex, share = as.vector(table(km$cluster)) / nrow(lab),
                    L = round(km$centers[, 1]))
  out[order(-out$share), ]
}

# Example:
# cl <- extract_lab_clusters("data-raw/photos/canada_warbler.jpg", k = 8, crop = "600x500+300+150")
# cl
# borealfeathers::print_pal(cl$hex)
#
# Pick colours by eye from cl$hex. Small but characteristic patches (a speculum,
# an eye-ring, a bill) often have a tiny share, so look past the top clusters,
# or crop that patch on its own and run again with k = 3.

# Alternative: colorfindr does a similar job in RGB space.
# colorfindr::get_colors("data-raw/photos/sora.jpg") |> colorfindr::make_palette(n = 6)
