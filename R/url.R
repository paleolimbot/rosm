
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



multi_file_download_async <- function(url, dest) {
  stopifnot(
    is.character(url),
    is.character(dest)
  )

  # recycle url along dest (could want to query one url many times)
  url <- rep_len(url, length(dest))

  # create a mutable object that keeps track of success/failure
  results <- new.env(parent = emptyenv())

  if (length(url) == 0) {
    return(invisible(character(0)))
  }

  pool <- curl::new_pool(total_con = 6, host_con = 6)
  pb <- progress::progress_bar$new(
    "[:bar] :file",
    total = length(url)
  )
  pb$tick(0)
  key <- paste(url, dest)

  for (i in seq_along(url)) {
    results[[paste(url[i], dest[i])]] <- FALSE
    curl::curl_fetch_multi(
      url[i],
      multi_download_async_success(url[i], dest[i], results, pb),
      multi_download_async_failure(url[i], dest[i], results, pb),
      pool = pool
    )
  }

  # this will block as long as files are being downloaded
  curl::multi_run(pool = pool)

  n_error <- sum(!unlist(as.list(results)))

  if (n_error > 0) {
    files <- if (n_error != 1) "files" else "file"
    bad_urls <- paste0("'", url[!unlist(as.list(results)[key])], "'", collapse = "\n")
    stop(
      glue::glue("{ n_error }/{ length(url) } { files } failed to download:\n{ bad_urls }")
    )
  }

  invisible(dest)
}

multi_download_async_success <- function(url, dest, results, pb) {
  force(url)
  force(dest)
  force(results)
  force(pb)

  function(res) {
    pb$tick(tokens = list(file = basename(url)))

    if (res$status_code >= 300) {
      results[[paste(url, dest)]] <- FALSE
      return()
    }

    if (!dir.exists(dirname(dest))) dir.create(dirname(dest), recursive = TRUE)
    con <- file(dest, "wb")
    on.exit(close(con))

    writeBin(res$content, con)
    results[[paste(url, dest)]] <- TRUE
  }
}

multi_download_async_failure <- function(url, dest, results, pb) {
  force(url)
  force(dest)
  force(results)
  force(pb)

  function(msg) {
    pb$tick(tokens = list(file = basename(url)))
    results[[paste(url, dest)]] <- FALSE
  }
}
