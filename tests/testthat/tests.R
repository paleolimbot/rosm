
# tests

context("ROSM tests")

nsbox <- prettymapr::makebbox(47.2, -59.7, 43.3, -66.4)

skip_on_cran()
test_that("all tile sources load", {
  # test contributed by rCarto (cartography package)

  tiles <- data.frame(types = osm.types(), status = NA, stringsAsFactors = FALSE)

  # helpful to run this on travis builds, since they are not often
  skip_on_cran()
  expect_message({
    for(i in 1:nrow(tiles)) {
      status <- tryCatch(
        {
          osm.plot(nsbox, type = tiles[i,1], xlab = tiles[i,1])
          status <- "OK"
        },
        error=function(cond) {
          message(cond, '\n')
          status <- "error"
        }
      )
      tiles[i,"status"] <- status
    }
  })

  expect_true(all(tiles$status == "OK"))
})

test_that("all bing tile sources load", {
  # test contributed by rCarto (cartography package)

  tiles <- data.frame(types = bmaps.types(), status = NA,
                      stringsAsFactors = FALSE)

  expect_message({
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
  })

  expect_true(all(tiles$status == "OK"))
})

test_that("custom map types load tiles", {

  # this should throw a deprecation message but still work
  tile.url.darkmatter <- function(xtile, ytile, zoom) {
    paste0(paste("http://a.basemaps.cartocdn.com/dark_all",
                 zoom, xtile, ytile, sep="/"), ".png")
  }

  assign("tile.url.darkmatter", tile.url.darkmatter, .GlobalEnv)

  skip_on_cran()
  expect_message(osm.plot(nsbox, type="darkmatter"),
                 "Using functions as custom tile sources is deprecated. Use string formats instead.")

  rm("tile.url.darkmatter", envir = .GlobalEnv)

  # using string formats
  ts <- as.tile_source("http://a.basemaps.cartocdn.com/dark_all/${z}/${x}/${y}.png")
  expect_is(ts, "tile_source")

  expect_equal(ts$get_tile_url(2, 1, 3),
               "http://a.basemaps.cartocdn.com/dark_all/3/2/1.png")

  expect_message(osm.plot(nsbox, type=ts))

  # test registration
  register_tile_source(dark = "http://a.basemaps.cartocdn.com/dark_all/${z}/${x}/${y}.png")
  expect_true("dark" %in% names(rosm:::registered_sources))
  expect_is(rosm:::registered_sources$dark, "tile_source")

  expect_message(osm.plot(nsbox, type="dark"))

  # test setting of default
  set_default_tile_source("stamenbw")
  expect_equal(get_default_tile_source()$name, "stamenbw")
  # plot
  expect_message(osm.plot(nsbox))
  # reset tile source
  set_default_tile_source("osm")
})


# plot the whole world (still doesn't work)
# osm.plot(makebbox(89.9, 179.9, -89.9, -179.9), zoom=0)
# prettymap(osm.plot(makebbox(89.9, 179.9, -89.9, -179.9)))

test_that("wrap around situations warn the user", {
  americas <- prettymapr::zoombbox(prettymapr::makebbox(89.9, 179.9, -89.9, -179.9), 2, c(-100, 0))
  # note this doesn't work with project = FALSE
  skip_on_cran()
  expect_warning(osm.plot(americas, zoomin=1), "Attempting to plot wrap around tiles")
  alaska <- prettymapr::makebbox(71, -129, 51, 172)
  expect_warning(osm.plot(alaska), "Attempting to plot wrap around tiles")
  expect_warning(osm.plot(alaska, project = FALSE),
                 "Attempting to plot wrap around tiles")
})

test_that("osm.image returns an image with the required attrs", {
  skip_on_cran()
  img <- expect_message(osm.image(nsbox))
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

  withr::with_package("cartography", {
    skip_on_cran()
    for(country in c("PL", "PT")) {
      message("Testing country ", country)
      spdf <- nuts0.spdf[nuts0.spdf$id==country,]
      x <- expect_message(osm.raster(spdf, type="osm"))

      expect_is(x, "Raster")
      expect_true(spdf@proj4string@projargs == x@crs@projargs)

      # expect_null(raster::plotRGB(x))
      expect_silent(sp::plot(spdf, add=T))
    }

    # make more rasters (projected)
    x <- expect_message(osm.raster(nsbox, projection=sp::CRS("+init=epsg:26920"), crop=TRUE))
    expect_is(x, "Raster")
    expect_equal(x@crs@projargs, sp::CRS("+init=epsg:26920")@projargs)

    # expect_silent(raster::plotRGB(x))

    # make non-projected raster
    x <- expect_message(osm.raster(nsbox))
    expect_is(x, "Raster")
    expect_equal(x@crs@projargs, sp::CRS("+init=epsg:3857")@projargs)

    # expect_null(raster::plotRGB(x))

    # make cropped raster, ensure bounds are different
    x <- expect_message(osm.raster(nsbox))
    xcropped <- expect_message(osm.raster(nsbox, crop = TRUE))
    expect_false({
      boundsx <- raster::as.matrix(x@extent)
      boundscropped <- raster::as.matrix(xcropped@extent)
      all(boundsx == boundscropped)
    })
  })
})

test_that("osm rasters are written to disk", {
  skip_on_cran()
  test_file <- tempfile(fileext = ".tif")
  x <- expect_message(osm.raster(nsbox))
  expect_silent(osm.raster(x, filename=test_file, overwrite=TRUE))
  expect_true(file.exists(test_file))
  unlink(test_file)

  expect_message(osm.raster(nsbox, projection=sp::CRS("+init=epsg:26920"), crop=T,
             filename=test_file, overwrite=TRUE))
  expect_true(file.exists(test_file))
  unlink(test_file)
})

# add test for default cachedir
test_that("non-default cache directories are respected", {
  skip_on_cran()
  # see issue #3 and PR #4
  # start with clean cache

  default_cache <- "rosm.cache"
  if(dir.exists(default_cache)) unlink(default_cache, recursive = TRUE)

  # osm.plot with default cache
  expect_false(dir.exists(default_cache))
  expect_message(osm.plot(nsbox))
  expect_true(dir.exists(default_cache))
  unlink(default_cache, recursive = TRUE)

  # osm.plot with fusetiles = FALSE, default cache
  expect_false(dir.exists(default_cache))
  expect_message(osm.plot(nsbox, fusetiles = FALSE))
  expect_true(dir.exists(default_cache))
  unlink(default_cache, recursive = TRUE)

  # osm.image with default cache
  expect_false(dir.exists(default_cache))
  expect_message(osm.image(nsbox))
  expect_true(dir.exists(default_cache))
  unlink(default_cache, recursive = TRUE)

  # osm.plot with tempdir cache
  newcache <- tempfile()[1]
  expect_false(dir.exists(newcache))
  expect_message(osm.plot(nsbox, cachedir = newcache))
  # expect no default cache
  expect_false(dir.exists(default_cache))
  # expect temporary cache
  expect_true(dir.exists(newcache))
  # remove tempfile cache
  unlink(newcache, recursive = TRUE)

  # osm.plot(..fusetiles = FALSE) with tempdir cache
  newcache <- tempfile()[1]
  expect_false(dir.exists(newcache))
  expect_message(osm.plot(nsbox, cachedir = newcache, fusetiles = FALSE))
  # expect no default cache
  expect_false(dir.exists(default_cache))
  # expect temporary cache
  expect_true(dir.exists(newcache))
  # remove tempfile cache
  unlink(newcache, recursive = TRUE)

  # osm.image with tempdir cache
  newcache <- tempfile()[1]
  expect_false(dir.exists(newcache))
  expect_message(osm.image(nsbox, cachedir = newcache))
  # expect no default cache
  expect_false(dir.exists(default_cache))
  # expect temporary cache
  expect_true(dir.exists(newcache))
  # remove tempfile cache
  unlink(newcache, recursive = TRUE)

})

test_that("default cache directory is respected", {
  skip_on_cran()

  default_cache <- get_default_cachedir()
  # check that default cache is the correct default
  expect_equal(default_cache, "rosm.cache")
  # start with a clean cache
  if(dir.exists(default_cache)) unlink(default_cache, recursive = TRUE)

  # osm.plot with default cache
  expect_false(dir.exists(default_cache))
  expect_message(osm.plot(nsbox))
  expect_true(dir.exists(default_cache))
  unlink(default_cache, recursive = TRUE)

  # osm.plot with different default cache
  new_cache <- tempfile()[1]
  set_default_cachedir(new_cache)
  expect_identical(get_default_cachedir(), new_cache)

  expect_false(dir.exists(new_cache))
  expect_message(osm.plot(nsbox))
  expect_true(dir.exists(new_cache))
  unlink(new_cache, recursive = TRUE)

  # check reset of default cache
  set_default_cachedir(NULL)
  expect_equal(default_cache, "rosm.cache")
})

test_that("extract bbox", {

  sf_bbox <- setNames(c(-89.60465, 43.03507, -89.20378, 43.36607),
                      c("xmin", "ymin", "xmax", "ymax"))
  class(sf_bbox) <- "bbox"

  sp_bbox <- matrix(sf_bbox, ncol = 2,
                    dimnames=list(c("x", "y"), c("min", "max")))
  expect_equal(sp_bbox, extract_bbox(sf_bbox))

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
