
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

test_that("osm_tile_rct() works", {
  tiles <- osm_tile(osm_lnglat(-64, 45), 0:4)
  envelopes <- osm_tile_envelope(tiles)
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

test_that("osm_ensure_lnglat() works", {
  skip_if_not_installed("sf")

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
  points <- osm_tile_top_left(tiles)
  points_3857 <- wk::xy(
    c(-20037508.3427892, -20037508.3427892, -10018754.1713946,
      -10018754.1713946, -7514065.62854597),
    c(20037508.3427892, 20037508.3427892, 10018754.1713946,
      10018754.1713946, 7514065.62854597),
    crs = "EPSG:3857"
  )
  expect_equal(osm_ensure_lnglat(points_3857), points)
})

test_that("osm_ensure_native() works", {
  skip_if_not_installed("sf")

  expect_error(
    osm_ensure_lnglat(wk::xy(1, 2)),
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
  points <- osm_tile_top_left(tiles)
  points_3857 <- wk::xy(
    c(-20037508.3427892, -20037508.3427892, -10018754.1713946,
      -10018754.1713946, -7514065.62854597),
    c(20037508.3427892, 20037508.3427892, 10018754.1713946,
      10018754.1713946, 7514065.62854597),
    crs = "EPSG:3857"
  )
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
