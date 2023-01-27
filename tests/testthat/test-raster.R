
test_that("osm_raster produces errors and warnings when appropriate", {
  bounds <- wk::rct(
    -7514064, 5009380,
    -6261722, 6261715,
    crs = osm_crs_native()
  )

  expect_error(
    expect_message(
      osm_raster(bounds, "fileXX:///bad_url.png", quiet = NA),
      "^\\[500\\] fileXX"
    ),
    "0/1 tiles loaded"
  )

  expect_error(
    expect_message(
      osm_raster(bounds, "fileXX:///bad_url.png", quiet = FALSE),
      "^\\[500\\] fileXX"
    ),
    "0/1 tiles loaded"
  )

  expect_silent(osm_raster(bounds, "fileXX:///bad_url.png", quiet = TRUE))

  expect_error(
    expect_message(
      osm_raster(bounds, "file:///does_not_exist.png", quiet = NA),
      "^\\[404\\] file://"
    ),
    "0/1 tiles loaded"
  )

  expect_warning(
    osm_raster(bounds, osm_url_spec_example(), quiet = NA),
    "9/16 tiles loaded"
  )
})

test_that("osm_raster() produces the correct raster image", {
  bounds <- wk::rct(
    -7476083, 5349058,
    -6594103, 6243203,
    crs = osm_crs_native()
  )

  grd <- osm_raster(bounds, osm_url_spec_example())
  expect_s3_class(grd, "wk_grd")
  expect_identical(wk::wk_crs(grd), osm_crs_native())

  local_edition(3)
  # Not sure why this print()s
  vdiffr::expect_doppelganger("basic-osm-raster", plot(grd))
})

test_that("guess_content_type() works", {
  expect_identical(guess_content_type("xx.png"), "image/png")
  expect_identical(guess_content_type("xx.jpg"), "image/jpeg")
  expect_identical(guess_content_type("xx.jpeg"), "image/jpeg")
  expect_identical(guess_content_type("xx.tif"), "image/tiff")
  expect_identical(guess_content_type("xx.tiff"), "image/tiff")
  expect_error(guess_content_type("xx.exe"), "Can't guess content type")
})

test_that("get_native_raster_loader() works", {
  expect_type(get_native_raster_loader("image/png"), "closure")
  expect_type(get_native_raster_loader("image/jpeg"), "closure")
  expect_type(get_native_raster_loader("image/tiff"), "closure")
  expect_error(
    get_native_raster_loader("not supported"),
    "Can't guess nativeRaster loader"
  )
})
