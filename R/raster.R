
#' Load an Open Street Map image
#'
#' @inheritParams osm_url_load_async
#' @inheritParams osm_tile_covering
#' @param quiet Use `TRUE` for fewer messages or `FALSE` for more messages.
#'
#' @return A [wk::grd_rct()] whose data member is a nativeRaster.
#' @export
#'
#' @examples
#' bounds <- wk::rct(
#'   -7476083, 5349058,
#'   -6594103, 6243203,
#'   crs = osm_crs_native()
#' )
#'
#' (grd <- osm_raster(bounds, osm_url_spec_example()))
#' plot(grd)
#'
osm_raster <- function(bbox, spec, zoom = osm_zoom_num_tiles(6),
                       cache_spec = NULL, quiet = NA) {
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

  state$n <- 0L
  state$n_loaded <- 0L
  state$range_x <- x_tile_range
  state$range_y <- y_tile_range
  state$block_size <- spec$block_size
  state$loader <- loader
  state$quiet <- !identical(quiet, FALSE)

  callback <- osm_raster_callback(state)
  osm_url_load_async(
    tiles,
    spec,
    callback,
    cache_spec = cache_spec
  )

  if (state$n_loaded == 0 && !isTRUE(quiet)) {
    stop(
      glue::glue("0/{state$n} tiles loaded. Use `quiet = FALSE` to see error messages")
    )
  } else if (state$n_loaded != state$n && !isTRUE(quiet)) {
    warning(
      glue::glue("{state$n_loaded}/{state$n} tiles loaded.")
    )
  }

  tile_bbox <- wk::wk_bbox(osm_tile_envelope(tiles))
  class(state$template) <- "nativeRaster"
  wk::grd_rct(state$template, tile_bbox)
}

osm_raster_callback <- function(state) {
  force(state)

  function(tiles, res) {
    # Keep track of loaded/not loaded so we can error if everything fails
    state$n <- state$n + 1L

    # Errors aren't the end of the world here...some providers return a 404
    # if a tile should be blank. file:// urls will have a status code of 0
    # on success but will go through the error callback if the file isn't found,
    # hence trying to catch that here to simulate a 404.
    if (inherits(res, "osm_url_error") && grepl("Couldn't open file", res$msg)) {
      res$status_code <- 404
    } else if (inherits(res, "osm_url_error")) {
      if (!state$quiet) message(sprintf("[%s] %s\n %s", res$status_code, res$url, res$msg))
      return(FALSE)
    }

    if (res$status_code >= 300) {
      if (!state$quiet) message(sprintf("[%s] %s", res$status_code, res$url))
      return(FALSE)
    }

    if (!state$quiet) message(sprintf("[200] %s", res$url))

    native <- try(state$loader(res$content), silent = state$quiet)
    if (inherits(native, "try-error")) {
      return(FALSE)
    }

    # Keep track of loaded/not loaded so we can error if everything fails
    state$n_loaded <- state$n_loaded + 1L

    # The indexing is tricky here because nativeRaster is actually row-major
    # despite its dimensions and subset/subset-assign methods lying about it
    ix0 <- tiles$x - state$range_x[1]
    iy0 <- tiles$y - state$range_y[1]
    ipxx0 <- (ix0 * state$block_size[1])
    ipxy0 <- (iy0 * state$block_size[2])
    ipxx <- rep(ipxx0:(ipxx0 + state$block_size[1] - 1), state$block_size[2])
    ipxy <- rep(ipxy0:(ipxy0 + state$block_size[2] - 1), each = state$block_size[1])
    ipx <- ipxy * ncol(state$template) + ipxx + 1L

    state$template[ipx] <- native
  }
}

guess_content_type <- function(server_url) {
  before_query_string <- strsplit(server_url, "?", fixed = TRUE)[[1]][1]
  ext <- tools::file_ext(before_query_string)
  switch(
    ext,
    png = "image/png",
    jpg = "image/jpeg",
    jpeg = "image/jpeg",
    tif = "image/tiff",
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
