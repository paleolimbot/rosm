
# modified from http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#R

bmaps.quadkey <- function(tilex, tiley, zoom) {
  nzoom <- 2^zoom
  if (tilex < 0 || tilex >= nzoom) stop("xtile out of range: ", tilex)
  if (tiley < 0 || tiley >= nzoom) stop("ytile out of range: ", tilex)
  out <- ""
  keymap <- matrix(0:3, byrow = TRUE, ncol = 2)
  decx <- tilex / nzoom
  decy <- tiley / nzoom
  for (i in 1:zoom) {
    n <- 2^i
    x <- floor(decx * 2^i) - floor(decx * 2^(i - 1)) * 2
    y <- floor(decy * 2^i) - floor(decy * 2^(i - 1)) * 2
    out <- paste0(out, keymap[y + 1, x + 1])
  }
  out
}

tiles.bybbox <- function(bbox, zoom, epsg = 4326) {
  nwlatlon <- .tolatlon(bbox[1, 1], bbox[2, 2], epsg)
  selatlon <- .tolatlon(bbox[1, 2], bbox[2, 1], epsg)

  if (nwlatlon[1] < -180) { # fixes wraparound problem with project=F
    nwlatlon[1] <- nwlatlon[1] + 360
  }

  if (nwlatlon[1] > selatlon[1]) {
    # wrapping around backside of earth
    backsidebbox <- matrix(c(nwlatlon[1], selatlon[2], 180, nwlatlon[2]), ncol = 2, byrow = FALSE)
    backsidetiles <- tiles.bybbox(backsidebbox, zoom, 4326)
    nwlatlon[1] <- -180
  } else {
    backsidetiles <- NULL
  }

  nw <- tile.xy(nwlatlon[1], nwlatlon[2], zoom)
  se <- tile.xy(selatlon[1], selatlon[2], zoom)
  tiles <- expand.grid(x = nw[1]:se[1], y = nw[2]:se[2])

  if (is.null(backsidetiles)) {
    tiles
  } else {
    rbind(backsidetiles, tiles)
  }
}

tile.xy <- function(x, y, zoom, epsg = 4326) {
  latlon <- .tolatlon(x, y, epsg)

  lat_rad <- latlon[2] * pi / 180
  n <- 2.0^zoom
  xtile <- floor((latlon[1] + 180.0) / 360.0 * n)
  ytile <- floor((1.0 - log(tan(lat_rad) + (1 / cos(lat_rad))) / pi) / 2.0 * n)
  xtile <- max(0, min(xtile, n - 1)) # limit x, y tiles to valid tile numbers
  ytile <- max(0, min(ytile, n - 1))
  c(xtile, ytile)
}

tile.nw <- function(xtile, ytile, zoom, epsg = 4326) {
  n <- 2.0^zoom
  lon_deg <- xtile / n * 360.0 - 180.0
  lat_rad <- atan(sinh(pi * (1 - 2 * ytile / n)))
  lat_deg <- lat_rad * 180 / pi

  projected <- .fromlatlon(lon_deg, lat_deg, epsg)
  # added to support wrap around tiles
  if (lon_deg < -180 && projected[1] > 0) {
    # wrap around situation

    maxx <- .fromlatlon(180, lat_deg, epsg)[1]
    projected[1] <- projected[1] - maxx * 2
  }
  projected
}

tile.bbox <- function(xtile, ytile, zoom, epsg = 4326) {
  nw <- tile.nw(xtile, ytile, zoom, epsg)
  se <- tile.nw(xtile + 1, ytile + 1, zoom, epsg)
  matrix(c(nw[1], se[2], se[1], nw[2]),
    ncol = 2,
    byrow = FALSE, dimnames = list(c("x", "y"), c("min", "max"))
  )
}

tile.url <- function(xtile, ytile, zoom, type) {
  fun <- type$get_tile_url
  if ("quadkey" %in% names(formals(fun))) {
    fun(xtile, ytile, zoom, quadkey = bmaps.quadkey(xtile, ytile, zoom))
  } else {
    fun(xtile, ytile, zoom)
  }
}

tile.ext <- function(type) {
  type$get_extension()
}

tile.maxzoom <- function(type) {
  type$get_max_zoom()
}

tile.maxzoom.default <- function() {
  return(19)
}

tile.minzoom.default <- function() {
  return(0)
}

tile.attribute <- function(type) {
  attribution <- type$get_attribution()
  if (!is.null(attribution)) message(attribution)
}

tile.cachename <- function(xtile, ytile, zoom, type, cachedir = NULL) {
  folder <- tile.cachedir(type, cachedir)
  ext <- tile.ext(type)
  file.path(folder, paste0(zoom, "_", xtile, "_", ytile, ".", ext))
}

tile.download <- function(tiles, zoom, type = "osm", forcedownload = FALSE, cachedir = NULL,
                          progress = "text", quiet = TRUE, pause = 0.1) {
  if (!forcedownload) {
    # check which tiles exist
    texists <- vapply(1:nrow(tiles), function(i) {
      cachename <- tile.cachename(tiles[i, 1], tiles[i, 2], zoom, type, cachedir)
      return(file.exists(cachename))
    }, logical(1))
    tiles <- tiles[!texists, , drop = FALSE]
  }

  if (nrow(tiles) > 0) {
    if (progress != "none") {
      message("Fetching ", nrow(tiles), " missing tiles")
      pb <- utils::txtProgressBar(min = 0, max = nrow(tiles), width = 20, file = stderr())
    }

    tile.apply(tiles, fun = function(xtile, ytile) {
      cachename <- tile.cachename(xtile, ytile, zoom, type, cachedir)
      url <- tile.url(xtile, ytile, zoom, type)
      # pause to avoid overwhelming servers
      if (pause > 0) Sys.sleep(pause)

      if (!quiet) message("trying URL: ", url)

      # try to download file
      result <- try(curl::curl_download(url, cachename, quiet = quiet, mode = "wb"),
        silent = quiet
      )

      # display errors only in progress mode
      if (!quiet && inherits(result, "try-error")) {
        message(sprintf(
          "Failed to download tile %s:(%s, %s) for type %s / %s",
          zoom, xtile, ytile, type$name, result
        ))
      } else if (!quiet && !file.exists(cachename)) {
        message(sprintf(
          "Failed to download tile %s:(%s, %s) for type %s",
          zoom, xtile, ytile, type$name
        ))
      }

      # delete files for try-errors
      if (inherits(result, "try-error")) {
        try(unlink(cachename), silent = TRUE)
      }
    }, progress = progress)

    if (progress != "none") message("...complete!")
  }
}
