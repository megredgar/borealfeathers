is_hex <- function(x) grepl("^#[0-9A-Fa-f]{6}$", x)

test_that("every stored colour is a valid hex code", {
  expect_true(all(is_hex(unlist(boreal_palettes))))
  expect_true(all(is_hex(unlist(boreal_continuous))))
  expect_true(all(is_hex(boreal_palette_info$hex)))
})

test_that("palette names are unique across discrete and continuous", {
  expect_length(intersect(names(boreal_palettes), names(boreal_continuous)), 0)
})

test_that("boreal_palette_info matches boreal_palettes", {
  for (nm in names(boreal_palettes)) {
    expect_identical(boreal_palette_info$hex[boreal_palette_info$palette == nm],
                     unname(boreal_palettes[[nm]]), info = nm)
  }
})

test_that("get_pal returns the right number of unnamed colours", {
  expect_length(get_pal("canada_warbler"), length(boreal_palettes$canada_warbler))
  expect_length(get_pal("sora", 3), 3)
  expect_length(get_pal("teal_seq"), 256)
  expect_length(get_pal("snipe_teal_div", 9), 9)
  expect_null(names(get_pal("sora")))
})

test_that("reverse and errors work", {
  expect_identical(get_pal("sora", reverse = TRUE), rev(get_pal("sora")))
  expect_error(get_pal("not_a_bird"), "Unknown palette")
  expect_warning(get_pal("sora", 10), "interpolating")
})

test_that("every discrete palette has a group, and list_palettes filters by it", {
  info <- unique(boreal_palette_info[c("palette", "group")])
  expect_setequal(info$palette, names(boreal_palettes))
  expect_false(anyNA(info$group))
  expect_true(all(list_palettes("shorebirds")$group == "shorebirds"))
  expect_equal(nrow(list_palettes("continuous")), length(boreal_continuous))
  expect_error(list_palettes("raptors"), "Unknown group")
})

test_that("ggplot2 scales build", {
  skip_if_not_installed("ggplot2")
  expect_s3_class(scale_colour_boreal("sora"), "ScaleDiscrete")
  expect_s3_class(scale_fill_boreal("teal_seq", discrete = FALSE), "ScaleContinuous")
})
