#' Get a boreal bird palette
#'
#' @param name Palette name. See [list_palettes()].
#' @param n Number of colours. For discrete palettes, the first `n` colours are
#'   returned; asking for more than the palette holds interpolates (with a
#'   warning). For continuous palettes, `n` evenly spaced colours (default 256).
#' @param type `"discrete"` or `"continuous"`. Defaults to the palette's own type.
#'   A discrete palette with `type = "continuous"` is interpolated through all
#'   of its colours in their stored order.
#' @param reverse Reverse the colour order?
#'
#' @return An unnamed character vector of hex colours.
#' @examples
#' get_pal("canada_warbler")
#' get_pal("blue_winged_teal", 3)
#' get_pal("snipe_teal_div", 9)
#' @export
get_pal <- function(name, n = NULL, type = NULL, reverse = FALSE) {
  pal <- lookup_pal(name)
  type <- if (is.null(type)) pal$type else match.arg(type, c("discrete", "continuous"))
  cols <- pal$cols

  if (type == "discrete") {
    if (is.null(n)) n <- length(cols)
    if (n > length(cols)) {
      warning(sprintf("'%s' has %d colours; interpolating to %d. Interpolated colours can be hard to tell apart.",
                      name, length(cols), n), call. = FALSE)
      out <- ramp(cols, n)
    } else if (pal$type == "continuous") {
      out <- ramp(cols, n)          # spread n colours along a continuous palette
    } else {
      out <- cols[seq_len(n)]
    }
  } else {
    if (is.null(n)) n <- 256
    out <- ramp(cols, n)
  }

  if (reverse) out <- rev(out)
  unname(out)
}

#' List available palettes
#'
#' @param group Optional group(s) to filter by: `"waterbirds"`, `"shorebirds"`,
#'   `"warblers"`, `"landbirds"`, `"neutrals"` or `"continuous"`.
#'
#' @return A data frame with palette name, group, type, number of stored
#'   colours and species.
#' @examples
#' list_palettes()
#' list_palettes("shorebirds")
#' list_palettes(c("neutrals", "continuous"))
#' @export
list_palettes <- function(group = NULL) {
  info <- unique(boreal_palette_info[c("palette", "group", "species")])
  d <- data.frame(
    palette = names(boreal_palettes),
    stringsAsFactors = FALSE
  )
  d$group <- info$group[match(d$palette, info$palette)]
  d$type <- "discrete"
  d$n <- lengths(boreal_palettes, use.names = FALSE)
  d$species <- info$species[match(d$palette, info$palette)]
  cont <- data.frame(
    palette = names(boreal_continuous),
    group = "continuous",
    type = ifelse(grepl("_div$", names(boreal_continuous)), "diverging", "sequential"),
    n = NA_integer_,
    species = NA_character_,
    stringsAsFactors = FALSE
  )
  out <- rbind(d, cont)
  if (!is.null(group)) {
    bad <- setdiff(group, unique(out$group))
    if (length(bad)) stop("Unknown group(s): ", paste(bad, collapse = ", "),
                          ". Choose from: ", paste(unique(out$group), collapse = ", "), call. = FALSE)
    out <- out[out$group %in% group, ]
  }
  rownames(out) <- NULL
  out
}

lookup_pal <- function(name) {
  if (!is.character(name) || length(name) != 1) stop("`name` must be a single palette name.", call. = FALSE)
  if (name %in% names(boreal_palettes)) return(list(cols = boreal_palettes[[name]], type = "discrete"))
  if (name %in% names(boreal_continuous)) return(list(cols = boreal_continuous[[name]], type = "continuous"))
  stop(sprintf("Unknown palette '%s'. See list_palettes().", name), call. = FALSE)
}

ramp <- function(cols, n) grDevices::colorRampPalette(cols, space = "Lab")(n)
