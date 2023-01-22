
test_that("osm_raster works", {
  bounds <- wk::rct(
    252185, 4815826, 739729, 5210280,
    crs = "EPSG:32620"
  )

  osm_raster(bounds, osm_url_spec_example())
})
