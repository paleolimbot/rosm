
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

  spec_with_default_name <- as_osm_url_spec(spec$server_url, name = "another name")
  expect_identical(
    spec_with_default_name,
    osm_url_spec(name = "another name")
  )

  spec_with_default_name <- as_osm_url_spec(spec, name = "another name")
  expect_identical(
    spec_with_default_name,
    osm_url_spec(name = "another name")
  )
})

test_that("urls can be generated from the default spec", {
  tiles <- data.frame(
    x = c(20, 21, 20, 21),
    y = c(22, 22, 23, 23),
    zoom = 6
  )

  expect_identical(
    osm_url(tiles, osm_url_spec()),
    c(
      "https://tile.openstreetmap.org/6/20/22.png",
      "https://tile.openstreetmap.org/6/21/22.png",
      "https://tile.openstreetmap.org/6/20/23.png",
      "https://tile.openstreetmap.org/6/21/23.png"
    )
  )
})

test_that("urls with quadkeys can be generated", {
  tiles <- data.frame(
    x = c(20, 21, 20, 21),
    y = c(22, 22, 23, 23),
    zoom = 6
  )

  expect_identical(
    osm_url(tiles, "http://something.com/${q}"),
    c(
      "http://something.com/030320",
      "http://something.com/030321",
      "http://something.com/030322",
      "http://something.com/030323"
    )
  )
})

test_that("urls with names can be generated", {
  spec <- osm_url_spec("${name}/${z}/${x}/${y}.png", name = "the_name")
  tiles <- data.frame(x = 1, y = 2, zoom = 3)
  expect_identical(
    osm_url(tiles, spec),
    "the_name/3/1/2.png"
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

test_that("url async loader works", {
  tiles <- data.frame(
    x = c(20, 21, 20, 21),
    y = c(22, 22, 23, 23),
    zoom = 6
  )

  spec <- osm_url_spec_example()
  spec$name <- "ex"

  # default does nothing but should write to the cache
  temp_cache <- tempfile()

  cache_spec <- paste0(temp_cache, "/", "${name}_${z}_${x}_${y}.png")
  expect_identical(
    osm_url_load_async(tiles, spec, cache_spec = cache_spec),
    tiles
  )

  expect_setequal(
    list.files(temp_cache),
    c("ex_6_20_22.png", "ex_6_20_23.png", "ex_6_21_22.png", "ex_6_21_23.png")
  )

  unlink(temp_cache, recursive = TRUE)
})

test_that("url async loader can load zero tiles", {
  tiles <- data.frame(x = double(), y = double(), zoom = double())
  expect_identical(osm_url_load_async(tiles, osm_url_spec_example()), tiles)
})

test_that("url async loader errors for paths that are urls", {
  tiles <- data.frame(
    x = c(20, 21, 20, 21),
    y = c(22, 22, 23, 23),
    zoom = 6
  )

  expect_error(
    osm_url_load_async(tiles, osm_url_spec_example(), cache_spec = "http://this.is.a.url"),
    "Cache results must be paths and not URLs"
  )
})

test_that("url async loader runs the error callback", {
  tiles <- data.frame(
    x = c(20, 21, 20, 21),
    y = c(22, 22, 23, 23),
    zoom = 6
  )

  callback <- function(tiles, res) {
    expect_equal(res$status_code, 500)
    stop(glue::glue("<{res$url}> {res$msg}"))
  }

  expect_error(
    osm_url_load_async(tiles, "this_is_not_a_file_anywhere", callback),
    "<file://this_is_not_a_file_anywhere>"
  )
})

test_that("url async loader runs the success callback", {
  tiles <- data.frame(
    x = c(20, 21, 20, 21),
    y = c(22, 22, 23, 23),
    zoom = 6
  )

  tiles_out <- data.frame(x = double(), y = double(), zoom = double())

  callback <- function(tiles, res) {
    expect_identical(res$status_code, 0L)
    expect_identical(nrow(tiles), 1L)
    tiles_out <<- rbind(tiles_out, tiles)
  }

  expect_identical(
    osm_url_load_async(tiles, osm_url_spec_example(), callback = callback),
    tiles
  )

  expect_identical(tiles_out[order(tiles_out$y, tiles_out$x), ], tiles)
})

test_that("url async loader stops for callback error", {
  tiles <- data.frame(
    x = c(20, 21, 20, 21),
    y = c(22, 22, 23, 23),
    zoom = 6
  )

  # After a callback error, nothing should be cached
  temp_cache <- tempfile()
  cache_spec <- paste0(temp_cache, "/", "${z}_${x}_${y}.png")

  callback <- function(tiles, res) {
    stop("In the name of Open Street Map!")
  }

  expect_error(
    osm_url_load_async(tiles, osm_url_spec_example(), callback = callback),
    "In the name of Open Street Map!"
  )

  expect_false(dir.exists(temp_cache))
})
