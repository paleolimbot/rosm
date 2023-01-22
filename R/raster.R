
osm_raster <- function(bbox, spec, zoom = osm_zoom_num_tiles(6),
                       cache_spec = NULL) {
  spec <- as_osm_url_spec(spec)
  if (is.na(spec$content_type)) {
    content_type <- guess_content_type(spec$server_url)
  } else {
    content_type <- spec$content_type
  }

  loader <- get_native_raster_loader(content_type)

  tiles <- osm_tile_covering(bbox, zoom)

  if (nrow(tiles) == 0) {
    empty_native_raster <- structure(integer(), class = "nativeRaster")
    return(wk::grd_rct(matrix(empty_native_raster, nrow = 0, ncol = 0), bbox))
  }

  x_tile_range <- range(tiles$x)
  y_tile_range <- range(tiles$y)
  tiles_x <- diff(x_tile_range) + 1L
  tiles_y <- diff(y_tile_range) + 1L
  pixels_x <- spec$block_size[1] * tiles_x
  pixels_y <- spec$block_size[2] * tiles_y

  state <- new.env(parent = emptyenv())
  state$template <- matrix(
    integer(pixels_x * pixels_y),
    ncol = pixels_x,
    nrow = pixels_y
  )

  state$n_loaded <- 0
  state$range_x <- x_tile_range
  state$range_y <- y_tile_range
  state$tiles_x <- tiles_x
  state$tiles_y <- tiles_y
  state$block_size <- spec$block_size
  state$loader <- loader

  callback <- osm_raster_callback(state)
  osm_url_load_async(
    tiles,
    spec,
    callback,
    cache_spec = cache_spec
  )

  class(state$template) <- "nativeRaster"
  wk::grd_rct(state$template, bbox)
}

osm_raster_callback <- function(state) {
  force(state)
  function(tiles, res) {
    native <- state$loader(res$content)

    ix0 <- tiles$x - state$range_x[1]
    iy0 <- tiles$y - state$range_y[1]
    ipxx0 <- (ix0 * state$block_size[1])
    ipxy0 <- (iy0 * state$block_size[2])
    ipxx <- rep(ipxx0:(ipxx0 + state$block_size[1] - 1), state$block_size[2])
    ipxy <- rep(ipxy0:(ipxy0 + state$block_size[2] - 1), each = state$block_size[1])
    ipx <- ipxy * ncol(state$template) + ipxx + 1L
    state$template[ipx] <- native

    state$n_loaded <- state$n_loaded + 1L
  }
}

guess_content_type <- function(server_url) {
  before_query_string <- strsplit(server_url, "?", fixed = TRUE)[[1]][1]
  ext <- tools::file_ext(before_query_string)
  switch(
    ext,
    png = "image/png",
    jpeg = "image/jpeg",
    tiff = "image/tiff",
    stop(sprintf("Can't guess content type from server url '%s'", server_url))
  )
}

get_native_raster_loader <- function(content_type) {
  switch(
    content_type,
    "image/png" = function(x) png::readPNG(x, native = TRUE),
    "image/jpeg" = function(x) jpeg::readJPEG(x, native = TRUE),
    "image/tiff" = function(x) tiff::readTIFF(x, native = TRUE),
    stop(sprintf("Can't guess nativeRaster loader from content type '%s'", content_type))
  )
}
