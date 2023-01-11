
#' Tile URL Specification
#'
#' @param server_url A url using `${x}`, `${y}`, and `${z}` for the x, y, and
#'   zoom level to be replaced. This can be any URL; non-URLs are assumed to be
#'   local file paths relative to the current working directory at the time of
#'   the download.
#' @param block_size The pixel size of each image
#' @param min_zoom,max_zoom The min/max zoom that this tile specification can handle
#' @param x An object to convert to an osm_url_spec
#' @param ... Passed to S3 methods
#'
#' @return An object of class osm_url_spec.
#' @export
#'
#' @examples
#' osm_url_spec()
#'
osm_url_spec <- function(server_url = "https://tile.openstreetmap.org/${z}/${x}/${y}.png",
                         block_size = c(256, 256),
                         min_zoom = 0,
                         max_zoom = 18) {
  stopifnot(
    is.character(server_url), length(server_url) == 1L,
    is.numeric(block_size), length(block_size) == 2L, all(is.finite(block_size)),
    is.numeric(min_zoom), length(min_zoom) == 1L, !is.na(min_zoom),
    is.numeric(max_zoom), length(max_zoom) == 1L, !is.na(max_zoom)
  )

  structure(
    list(
      server_url = server_url,
      block_size = as.integer(block_size),
      min_zoom = as.double(min_zoom),
      max_zoom = as.double(max_zoom)
    ),
    class = "osm_url_spec"
  )
}

#' @rdname osm_url_spec
#' @export
osm_url_spec_example <- function() {
  base <- system.file("extdata/osmns", package = "rosm")
  osm_url_spec(paste0(base, "/${z}_${x}_${y}.png"))
}

#' @rdname osm_url_spec
#' @export
as_osm_url_spec <- function(x, ...) {
  UseMethod("as_osm_url_spec")
}

#' @export
as_osm_url_spec.osm_url_spec <- function(x, ...) {
  x
}

#' @export
as_osm_url_spec.character <- function(x, ...) {
  osm_url_spec(x)
}

#' Resolve a tile into a URL
#'
#' @param spec An [osm_url_spec()]
#' @inheritParams osm_tile
#'
#' @return A character vector of URLs
#' @export
#'
#' @examples
#' bounds <- wk::rct(
#'   -7514064, 5009380,
#'   -6261722, 6261715,
#'   crs = osm_crs_native()
#' )
#'
#' tiles <- osm_tile_covering(bounds, zoom = 6)
#' osm_url(tiles, osm_url_spec())
#'
osm_url <- function(tile, spec) {
  tile <- osm_tile_normalize(tile)
  spec <- as_osm_url_spec(spec)

  tile <- tile[c("x", "y", "zoom")]
  names(tile) <- c("x", "y", "z")

  glue_data <- as.environment(tile)
  glue::glue_safe(
    spec$server_url,
    .open = "${",
    .close = "}",
    .na = NULL,
    .envir = glue_data
  )
}

#' Load tile URLs
#'
#' @inheritParams osm_url
#' @inheritParams osm_tile
#' @param cache_spec An optional [osm_url_spec()] or character vector to be
#'   used as the cache.
#' @param callback A function to be run for each tile fetch or NULL
#'   to do nothing. The callback is always called with two arguments: the first
#'   is the subset of `tile` for which this URL applies (typically one row but
#'   can be more than one in some corner cases); the second is the curl
#'   response object whose useful elements are url, status_code, type, and
#'   content.
#'
#' @return `tile`, invisibly.
#' @export
#'
#' @examples
#' bounds <- wk::rct(
#'   252185, 4815826, 739729, 5210280,
#'   crs = "EPSG:32620"
#' )
#'
#' tiles <- osm_tile_covering(bounds, zoom = 5)
#'
#' osm_url_load_async(
#'   tiles,
#'   osm_url_spec_example(),
#'   function(tile, res) {
#'     str(tile)
#'     str(res)
#'   }
#' )
#'
osm_url_load_async <- function(tile, spec, callback = NULL, cache_spec = NULL) {
  tile <- ensure_tile(tile)
  spec <- as_osm_url_spec(spec)
  cache_spec <- if (is.null(cache_spec)) osm_url_spec(NA_character_) else as_osm_url_spec(cache_spec)
  callback <- if (is.null(callback)) function(...) NULL else as.function(callback)

  # calculate the urls and cache values
  tile_url <- osm_url(tile, spec)
  tile_normalized_unique <- which(!duplicated(tile_url) & !is.na(tile_url))

  if (length(tile_normalized_unique) == 0) {
    return(invisible(tile))
  }

  urls <- tile_url[tile_normalized_unique]
  cached <- osm_url(tile[tile_normalized_unique, , drop = FALSE], cache_spec)
  tile_url_id <- match(tile_url, urls)

  # make sure urls are urls (e.g., with file://)
  urls <- ensure_url(urls)

  # make sure cache values are paths
  cached <- ensure_path(cached)

  # replace urls where the cached path exists with a file:// url
  cache_hit <- file.exists(cached)
  cache_hit[is.na(cache_hit)] <- FALSE
  cached_as_url <- ensure_url(cached)
  urls_with_cache <- urls
  urls_with_cache[cache_hit] <- cached_as_url[cache_hit]
  cached[cache_hit] <- NA_character_

  # use curl's async downloader to kick off loads for all tiles in parallel
  # evaluating callback for each as they are completed
  pool <- curl::new_pool(total_con = 6, host_con = 6)
  pb <- progress::progress_bar$new(
    "[:bar]",
    total = length(urls)
  )
  pb$tick(0)
  state <- as.environment(
    list(
      pb = pb,
      tile = tile,
      tile_url = tile_url,
      callback = callback
    )
  )

  for (i in seq_along(urls)) {
    curl::curl_fetch_multi(
      urls_with_cache[i],
      multi_download_async_success(urls[i], cached[i], state),
      multi_download_async_failure(urls[i], cached[i], state),
      pool = pool
    )
  }

  # this will block as long as files are being downloaded
  curl::multi_run(pool = pool)

  invisible(tile)
}

is_url <- function(x) {
  grepl("://", x)
}

ensure_url <- function(x) {
  ifelse(
    is_url(x),
    x,
    paste0("file://", normalizePath(x, winslash = "/", mustWork = FALSE))
  )
}

ensure_path <- function(x) {
  if (any(is_url(x))) {
    stop("Cache results must be paths and not URLs")
  }

  x
}

multi_download_async_success <- function(url, cached, state) {
  force(url)
  force(cached)
  force(state)

  function(res) {
    state$pb$tick()
    tiles <- state$tile[!is.na(state$tile_url) & (state$tile_url == url), , drop = FALSE]
    state$callback(tiles, res)

    # Only write to the cache if the callback succeeds
    if (!is.na(cached)) {
      if (!dir.exists(dirname(cached))) dir.create(dirname(cached), recursive = TRUE)
      con <- file(cached, "wb")
      on.exit(close(con))
      writeBin(res$content, con)
    }
  }
}

multi_download_async_failure <- function(url, cached, state) {
  force(url)
  force(cached)
  force(state)

  function(msg) {
    state$pb$tick()
    stop(glue::glue("<{url}>: {msg}"))
  }
}
