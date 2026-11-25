# Check every discrete palette for colour-vision-deficiency (CVD) robustness and
# lightness spread. Run after editing R/palettes.R.
#
# For each palette, it reports the smallest CIEDE2000 difference between any two
# colours under typical vision and simulated deuteranopia, protanopia and
# tritanopia (Machado et al. 2009, severity 1). Pairs below ~10 are hard to tell
# apart in a legend; below ~5 they are effectively the same colour.

# install.packages(c("colorspace", "farver"))
devtools::load_all()

sim <- list(
  normal = identity,
  deutan = function(x) colorspace::deutan(x, severity = 1),
  protan = function(x) colorspace::protan(x, severity = 1),
  tritan = function(x) colorspace::tritan(x, severity = 1)
)

min_de <- function(cols) {
  m <- farver::decode_colour(cols)
  d <- farver::compare_colour(m, from_space = "rgb", method = "cie2000")
  d[upper.tri(d, diag = TRUE)] <- NA
  i <- which(d == min(d, na.rm = TRUE), arr.ind = TRUE)[1, ]
  sprintf("%5.1f (%d,%d)%s", min(d, na.rm = TRUE), i[2], i[1], if (min(d, na.rm = TRUE) < 10) " !" else "  ")
}

res <- t(sapply(boreal_palettes, function(p) sapply(sim, function(f) min_de(f(p)))))
lightness <- sapply(boreal_palettes, function(p) {
  L <- farver::convert_colour(farver::decode_colour(p), "rgb", "lab")[, "l"]
  paste(sort(round(L)), collapse = " ")
})
print(cbind(as.data.frame(res), `L* (sorted)` = lightness))

# Visual check
for (nm in names(boreal_palettes)) {
  colorspace::swatchplot(boreal_palettes[nm], cvd = TRUE)   # named list -> labelled rows
  readline("Enter for next palette...")
}

# Continuous palettes: lightness should be monotonic (sequential) or
# symmetric around a light midpoint (diverging).
for (nm in names(boreal_continuous)) colorspace::specplot(get_pal(nm, 101), main = nm)
