
test_that("default URL spec works", {
  spec <- osm_url_spec()
  expect_identical(spec$block_size, c(256L, 256L))
  expect_identical(spec$min_zoom, 0)
  expect_identical(spec$max_zoom, 18)
})

test_that("example spec works", {
  spec <- osm_url_spec_example()
  expect_match(spec$server_url, "extdata/osmns/${z}_${x}_${y}.png", fixed = TRUE)
})

test_that("as_osm_url_spec() works", {
  spec <- osm_url_spec()
  expect_identical(as_osm_url_spec(spec), spec)
  expect_identical(as_osm_url_spec(spec$server_url), spec)
})

test_that("urls can be generated from the default spec", {
  tiles <- data.frame(
    x = c(20, 21, 20, 21),
    y = c(22, 22, 23, 23),
    zoom = 6
  )

  expect_identical(
    as.character(osm_url(tiles, osm_url_spec())),
    c(
      "https://tile.openstreetmap.org/6/20/22.png",
      "https://tile.openstreetmap.org/6/21/22.png",
      "https://tile.openstreetmap.org/6/20/23.png",
      "https://tile.openstreetmap.org/6/21/23.png"
    )
  )
})

test_that("tiles are normalized before generating URLs", {
  oversized_rect <- wk::rct(
    -20038000, -20038000,
    20038000, 20038000,
    crs = osm_crs_native()
  )

  tiles <- osm_tile_covering(oversized_rect, zoom = 0)
  expect_identical(
    unique(as.character(osm_url(tiles, osm_url_spec()))),
    c(NA, "https://tile.openstreetmap.org/0/0/0.png")
  )
})
