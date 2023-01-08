
test_that("osm_tile() works", {
  expect_identical(
    osm_tile(osm_lnglat(-64, 45), 1:5),
    data.frame(
      x = c(0, 1, 2, 5, 10),
      y = c(0, 1, 2, 5, 11),
      zoom = 1:5
    )
  )
})

test_that("osm_tile_covering() works for native bounds", {
  bounds <- wk::rct(
    -20037507, 1, -1, 20037507,
    crs = osm_crs_native()
  )

  expect_identical(
    osm_tile_covering(bounds, zoom = 1L),
    data.frame(
      x = 0L,
      y = 0L,
      zoom = 1L
    )
  )

  expect_identical(
    osm_tile_covering(bounds, zoom = 2L),
    data.frame(
      x = c(0L, 1L, 0L, 1L),
      y = c(0L, 0L, 1L, 1L),
      zoom = 2L
    )
  )
})

test_that("osm_tile_covering() works for lnglat bounds", {
  bounds <- wk::rct(
    -179.9, 0.1, -0.1, 85.05,
    crs = wk::wk_crs_longlat()
  )

  expect_identical(
    osm_tile_covering(bounds, zoom = 1L),
    data.frame(
      x = 0L,
      y = 0L,
      zoom = 1L
    )
  )

  expect_identical(
    osm_tile_covering(bounds, zoom = 2L),
    data.frame(
      x = c(0L, 1L, 0L, 1L),
      y = c(0L, 0L, 1L, 1L),
      zoom = 2L
    )
  )
})

test_that("osm_tile_covering() works for s2-style rectangle bounds", {
  bounds <- data.frame(
    lng_lo = 19,
    lat_lo = 41,
    lng_hi = -169,
    lat_hi = 81
  )

  bounds_rct <- wk::rct(19, 41, -169, 81, crs = wk::wk_crs_longlat())

  expect_identical(
    osm_tile_covering(bounds, zoom = 3L),
    osm_tile_covering(bounds_rct, zoom = 3L)
  )
})

test_that("osm_tile_covering() can compute a range for out of bounds lats", {
  bounds <- wk::rct(
    -179.9, -100, -0.1, 100,
    crs = wk::wk_crs_longlat()
  )

  expect_identical(
    osm_tile_covering(bounds, zoom = 1L),
    data.frame(
      x = c(0L, 0L),
      y = c(0L, 1L),
      zoom = 1L
    )
  )
})

test_that("osm_tile_covering() works for arbitrary CRS bounds", {
  bounds <- wk::rct(
    252185, 4815826, 739729, 5210280,
    crs = "EPSG:32620"
  )

  expect_identical(
    osm_tile_covering(bounds, zoom = 1L),
    data.frame(
      x = 0L,
      y = 0L,
      zoom = 1L
    )
  )

  expect_identical(
    osm_tile_covering(bounds, zoom = 6L),
    data.frame(
      x = c(20L, 21L, 20L, 21L),
      y = c(22L, 22L, 23L, 23L),
      zoom = 6L
    )
  )
})

test_that("osm_tile_covering() works with zoom as a function", {
  bounds <- wk::rct(
    -7514064, 5009380,
    -6261722, 6261715,
    crs = osm_crs_native()
  )

  covr1 <- osm_tile_covering(bounds, zoom = osm_zoom_num_tiles(1))
  expect_identical(nrow(covr1), 1L)

  covr4 <- osm_tile_covering(bounds, zoom = osm_zoom_num_tiles(4))
  expect_identical(nrow(covr4), 4L)

  covr16 <- osm_tile_covering(bounds, zoom = osm_zoom_num_tiles(16))
  expect_identical(nrow(covr16), 16L)

  bounds0 <- wk::rct(
    -6261722, 6261715,
    -6261722, 6261715,
    crs = osm_crs_native()
  )

  expect_error(
    osm_tile_covering(bounds0, function(...) FALSE),
    "`zoom` function returned FALSE for all zoom levels"
  )
})

test_that("osm_tile_envelope() works for lng/lat", {
  tiles <- osm_tile(osm_lnglat(-64, 45), 0:4)
  envelopes <- osm_tile_envelope(tiles, wk::wk_crs_longlat())
  expect_identical(wk::wk_crs(envelopes), wk::wk_crs_longlat())

  expect_identical(
    wk::rct_xmin(envelopes),
    c(-180, -180, -90, -90, -67.5)
  )

  expect_identical(
    wk::rct_xmax(envelopes),
    c(180, 0, 0, -45, -45)
  )
})

test_that("osm_tile_envelope() works for native crs", {
  tiles <- osm_tile(osm_lnglat(-64, 45), 0:4)
  envelopes <- osm_tile_envelope(tiles, osm_crs_native())
  expect_identical(wk::wk_crs(envelopes), osm_crs_native())

  top_left_lnglat <- osm_tile_top_left(tiles, wk::wk_crs_longlat())
  top_left_proj <- osm_ensure_native(top_left_lnglat)

  expect_equal(
    wk::rct_xmin(envelopes),
    wk::xy_x(top_left_proj)
  )

  expect_equal(
    wk::rct_ymax(envelopes),
    wk::xy_y(top_left_proj)
  )
})

test_that("osm_tile_envelope() errors for unsupported crs", {
  tiles <- osm_tile(osm_lnglat(-64, 45), 0:4)
  expect_error(
    osm_tile_envelope(tiles, "EPSG:32620"),
    "Unsupported `crs`"
  )
})

test_that("osm_ensure_lnglat() works", {
  expect_error(
    osm_ensure_lnglat(wk::xy(1, 2)),
    "Can't transform NULL"
  )

  expect_identical(
    osm_ensure_lnglat(wk::xy(1, 2, crs = wk::wk_crs_inherit())),
    osm_lnglat(1, 2)
  )

  expect_identical(
    osm_ensure_lnglat(osm_lnglat(1, 2)),
    osm_lnglat(1, 2)
  )

  tiles <- osm_tile(osm_lnglat(-64, 45), 0:4)
  points <- osm_tile_top_left(tiles, crs = wk::wk_crs_longlat())
  points_3857 <- wk::xy(
    c(-20037508.3427892, -20037508.3427892, -10018754.1713946,
      -10018754.1713946, -7514065.62854597),
    c(20037508.3427892, 20037508.3427892, 10018754.1713946,
      10018754.1713946, 7514065.62854597),
    crs = "EPSG:3857"
  )

  # uses the internal formulas with no sf
  expect_equal(osm_ensure_lnglat(points_3857), points)

  skip_if_not_installed("sf")

  # use a crs representation that rosm can't detect as native
  wk::wk_crs(points_3857) <- wk::wk_crs_projjson(sf::st_crs(3857))
  expect_false(crs_is_native(wk::wk_crs(points_3857)))

  expect_equal(osm_ensure_lnglat(points_3857), points)

})

test_that("osm_ensure_native() works", {
  expect_error(
    osm_ensure_native(wk::xy(1, 2)),
    "Can't transform NULL"
  )

  expect_identical(
    osm_ensure_native(wk::xy(1, 2, crs = wk::wk_crs_inherit())),
    osm_native(1, 2)
  )

  expect_identical(
    osm_ensure_native(osm_native(1, 2)),
    osm_native(1, 2)
  )

  tiles <- osm_tile(osm_lnglat(-64, 45), 0:4)
  points <- osm_tile_top_left(tiles, crs = wk::wk_crs_longlat())
  points_3857 <- wk::xy(
    c(-20037508.3427892, -20037508.3427892, -10018754.1713946,
      -10018754.1713946, -7514065.62854597),
    c(20037508.3427892, 20037508.3427892, 10018754.1713946,
      10018754.1713946, 7514065.62854597),
    crs = "EPSG:3857"
  )

  # uses the internal formulas with no sf
  expect_equal(osm_ensure_native(points), points_3857)

  skip_if_not_installed("sf")

  # use a crs representation that rosm can't detect as lnglat
  wk::wk_crs(points) <- wk::wk_crs_projjson(sf::st_crs(4326))
  expect_false(crs_is_lnglat(wk::wk_crs(points)))
  expect_equal(osm_ensure_native(points), points_3857)
})

test_that("ensure_tile() works", {
  expect_error(
    ensure_tile(data.frame()),
    "must have columns `x`, `y`, and `zoom`"
  )

  expect_error(
    ensure_tile(data.frame(x = double(), y = double(), zoom = character())),
    "must be numeric"
  )
})
