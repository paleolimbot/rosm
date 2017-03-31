
# tests
library(prettymapr)
library(rosm)
library(testthat)

context("ROSM tests")

nsbox <- makebbox(47.2, -59.7, 43.3, -66.4)

test_that("all tile sources load", {
  # test contributed by rCarto (cartography package)

  tiles <- data.frame(types = osm.types(), status = NA,
                      stringsAsFactors = FALSE)

  for(i in 1:nrow(tiles)) {
    status <- tryCatch(
      {
        osm.plot(nsbox, type = tiles[i,1])
        status <- "OK"
      },
      error=function(cond) {
        message(cond, '\n')
        status <- "error"
      }
    )
    tiles[i,"status"] <- status
  }

  expect_true(all(tiles$status == "OK"))
})

test_that("all bing tile sources load", {
  # test contributed by rCarto (cartography package)

  tiles <- data.frame(types = bmaps.types(), status = NA,
                      stringsAsFactors = FALSE)

  for(i in 1:nrow(tiles)) {
    status <- tryCatch(
      {
        bmaps.plot(nsbox, type = tiles[i,1])
        status <- "OK"
      },
      error=function(cond) {
        message(cond, '\n')
        status <- "error"
      }
    )
    tiles[i,"status"] <- status
  }

  expect_true(all(tiles$status == "OK"))
})

test_that("custom map types load tiles", {

  # this should throw a deprecation message but still work
  tile.url.darkmatter <- function(xtile, ytile, zoom) {
    paste0(paste("http://a.basemaps.cartocdn.com/dark_all",
                 zoom, xtile, ytile, sep="/"), ".png")
  }

  assign("tile.url.darkmatter", tile.url.darkmatter, .GlobalEnv)

  expect_message(osm.plot(nsbox, type="darkmatter"),
                 "Using functions as custom tile sources is deprecated. Use string formats instead.")

  rm("tile.url.darkmatter", envir = .GlobalEnv)

  # using string formats
  ts <- as.tile_source("http://a.basemaps.cartocdn.com/dark_all/${z}/${x}/${y}.png")
  expect_is(ts, "tile_source")

  expect_equal(ts$get_tile_url(2, 1, 3),
               "http://a.basemaps.cartocdn.com/dark_all/3/2/1.png")

  osm.plot(nsbox, type=ts)

  # test registration
  register_tile_source(dark = "http://a.basemaps.cartocdn.com/dark_all/${z}/${x}/${y}.png")
  expect_true("dark" %in% names(rosm:::registered_sources))
  expect_is(rosm:::registered_sources$dark, "tile_source")

  osm.plot(nsbox, type="dark")

  # test setting of default
  set_default_tile_source("stamenbw")
  expect_equal(get_default_tile_source()$name, "stamenbw")
  # plot
  osm.plot(nsbox)
  # reset tile source
  set_default_tile_source("osm")
})


# plot the whole world (still doesn't work)
# osm.plot(makebbox(89.9, 179.9, -89.9, -179.9), zoom=0)
# prettymap(osm.plot(makebbox(89.9, 179.9, -89.9, -179.9)))

test_that("wrap around situations warn the user", {
  americas <- zoombbox(makebbox(89.9, 179.9, -89.9, -179.9), 2, c(-100, 0))
  # note this doesn't work with project = FALSE
  expect_warning(osm.plot(americas), "Attempting to plot wrap around tiles")
  alaska <- makebbox(71, -129, 51, 172)
  expect_warning(osm.plot(alaska), "Attempting to plot wrap around tiles")
  expect_warning(osm.plot(alaska, project = FALSE),
                 "Attempting to plot wrap around tiles")
})

test_that("osm.image returns an image with the required attrs", {
  img <- osm.image(nsbox)
  expect_is(img, "array")
  expect_length(dim(img), 3)
  expect_true(dim(img)[3] %in% c(3, 4))

  attrs <- attributes(img)
  expect_true(all(c("bbox", "epsg", "type", "zoom", "tiles") %in% names(attrs)))
  expect_is(attrs$bbox, "matrix")
  expect_is(attrs$type, "tile_source")
  expect_is(attrs$epsg, "numeric")
  expect_is(attrs$zoom, "numeric")
})


# osm.raster
library(cartography)

test_that("osm.raster creates raster objects with the correct projection", {

  for(country in c("PL", "PT")) {
    message("Testing country ", country)
    spdf <- nuts0.spdf[nuts0.spdf$id==country,]
    x <- osm.raster(spdf, type="osm")

    expect_is(x, "Raster")
    expect_true(spdf@proj4string@projargs == x@crs@projargs)

    raster::plotRGB(x)
    plot(spdf, add=T)
  }

  # make more rasters (projected)
  x <- osm.raster(nsbox, projection=sp::CRS("+init=epsg:26920"), crop=TRUE)
  expect_is(x, "Raster")
  expect_equal(x@crs@projargs, sp::CRS("+init=epsg:26920")@projargs)

  raster::plotRGB(x)

  # make non-projected raster
  x <- osm.raster(nsbox)
  expect_is(x, "Raster")
  expect_equal(x@crs@projargs, sp::CRS("+init=epsg:3857")@projargs)

  raster::plotRGB(x)

  # make cropped raster, ensure bounds are different
  x <- osm.raster(nsbox)
  xcropped <- osm.raster(nsbox, crop = TRUE)
  boundsx <- raster::as.matrix(x@extent)
  boundscropped <- raster::as.matrix(xcropped@extent)

  expect_false(all(boundsx == boundscropped))
})

test_that("osm rasters are written to disk", {
  x <- osm.raster(nsbox, crop=TRUE)
  osm.raster(x, filename="test.tif")
  expect_true(file.exists("test.tif"))
  unlink("test.tif")

  osm.raster(nsbox, projection=CRS("+init=epsg:26920"), crop=T,
             filename="test.tif", overwrite=TRUE)
  expect_true(file.exists("test.tif"))
  unlink("test.tif")
})


# some wrap around situations currently don't work
# test_that("harder wrap around situations do not fail", {
#   americas <- zoombbox(makebbox(89.9, 179.9, -89.9, -179.9), 2, c(-100, 0))
#   # this works but is mega off
#   expect_warning(osm.raster(americas), "Attempting to plot wrap around tiles")
#   x <- osm.raster(americas)
#   xbox <- raster::as.matrix(x@extent)
#   # should be square ish
#   wh <- xbox[, 2] - xbox[, 1]
#   aspect <- wh[1] /  wh[2]
#   expect_lte(abs(1-aspect), 0.2)
#
#   # this one doesn't work at all
#   expect_warning(osm.plot(americas, project = FALSE),
#                  "Attempting to plot wrap around tiles")
# })
