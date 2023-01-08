
#' Low-level tile math
#'
#' Open Street Map operates using a system of tiles whose value and bounds
#' are easily calculated from WGS84 longitude/latitude. These functions convert
#' between tile system coordinates and longitude/latitude.
#'
#' @param pt A vector of points as coerced by [wk::as_xy()]. The CRS for these
#'   points is considered.
#' @param zoom A zoom level, generally between 0 and 21, with higher values
#'   representing a smaller (i.e., more detailed) tile.
#' @param crs A target CRS. Either [wk::wk_crs_longlat()] or
#'   [osm_crs_native()].
#' @param tile A `data.frame()` with columns `x`, `y`, and `zoom`.
#'
#' @return
#'   - `osm_tile()`: A `data.frame()` with columns `x`, `y`, and `zoom`.
#'   - `osm_tile_top_left()`: A [wk::xy()] of the top-left (northwest) corner
#'     of the tile.
#'   - `osm_tile_envelope()`: A [wk::rct()] of the tile bounds.
#' @export
#'
#' @examples
#' (tiles <- osm_tile(osm_lnglat(-64, 45), zoom = 0:5))
#' osm_tile_envelope(tiles)
#'
osm_tile <- function(pt, zoom) {
  lnglat <- unclass(osm_ensure_lnglat(pt))

  lat_rad <- lnglat[[2]] * pi / 180.0
  n <- 2.0^zoom
  xtile <- floor(n / 2 + osm_lng_degrees_to_native(lnglat[[1]], n / pi / 2))
  ytile <- floor(n / 2 - osm_lat_degrees_to_native(lnglat[[2]], n / pi / 2))
  data.frame(x = xtile, y = ytile, zoom = zoom)
}

#' @rdname osm_tile
#' @export
osm_tile_top_left <- function(tile, crs = osm_crs_native()) {
  ensure_tile(tile)
  n <- 2.0^tile$zoom

  if (crs_is_lnglat(crs)) {
    x <- osm_native_to_lng_degrees(tile$x - n / 2, n / pi / 2)
    y <- osm_native_to_lat_degrees(n / 2 - tile$y, n / pi / 2)
  } else if (crs_is_native(crs)) {
    x <- (tile$x / n - 0.5) * 2 * pi * 6378137
    y <- (0.5 - tile$y / n) * 2 * pi * 6378137
  } else {
    stop("Unsupported `crs`")
  }

  wk::xy(x, y, crs = crs)
}

#' @rdname osm_tile
#' @export
osm_tile_envelope <- function(tile, crs = osm_crs_native()) {
  ensure_tile(tile)
  top_left <- unclass(osm_tile_top_left(tile, crs = crs))
  bottom_right <- unclass(
    osm_tile_top_left(
      list(
        x = tile$x + 1,
        y = tile$y + 1,
        zoom = tile$zoom
      ),
      crs = crs
    )
  )

  wk::new_wk_rct(
    list(
      xmin = top_left$x,
      ymin = bottom_right$y,
      xmax = bottom_right$x,
      ymax = top_left$y
    ),
    crs = crs
  )
}

#' Get an OSM tile covering
#'
#' @param bbox A [wk::rct()] or object with a [wk::wk_bbox()] method.
#' @param zoom A zoom level or an auto zoom specifier like
#'   [osm_zoom_num_tiles()].
#' @param num_tiles The minimum number of tiles to use when choosing a
#'   zoom level.
#'
#' @return
#'   - `osm_tile_covering()` returns a `data.frame()` with columns x, y, and zoom.
#' @export
#'
#' @examples
#' bounds <- wk::rct(
#'   -7514064, 5009380,
#'   -6261722, 6261715,
#'   crs = osm_crs_native()
#' )
#'
#' osm_tile_covering(bounds)
#'
osm_tile_covering <- function(bbox, zoom = osm_zoom_num_tiles(6)) {
  # S2's s2_bounds_rect() gives a data.frame like this
  if (identical(names(bbox), c("lng_lo", "lat_lo", "lng_hi", "lat_hi"))) {
    names(bbox) <- c("xmin", "ymin", "xmax", "ymax")
    bbox <- wk::new_wk_rct(bbox, crs = wk::wk_crs_longlat())
  }

  crs <- wk::wk_crs(bbox)

  if (!inherits(bbox, "wk_rct")) {
    bbox <- wk::wk_bbox(bbox)
  }

  if (is.function(zoom)) {
    for (potential_zoom in 0:22) {
      potential_covering <- osm_tile_covering(bbox, potential_zoom)
      if (zoom(bbox, potential_covering)) {
        return(potential_covering)
      }
    }

    stop("`zoom` function returned FALSE for all zoom levels in 0:22")
  }

  if (crs_is_lnglat(crs)) {
    bbox <- unclass(bbox)
    # for these bboxes, xmin can be greater than xmax to wrap around a date line
    if (bbox$xmin > bbox$xmax) {
      bbox$xmin <- bbox$xmin - 180.0
    }

    # important to clamp latitude bounds or else we may get NaNs which prevent
    # us from getting a range of tile y values
    if (bbox$ymax > 85.0511287798065) {
      bbox$ymax <- 85.0511287798065
    }

    if (bbox$ymin < -85.0511287798065) {
      bbox$ymin <- -85.0511287798065
    }

    top_left <- osm_tile(wk::xy(bbox$xmin, bbox$ymax, crs = crs), zoom)
    bottom_right <- osm_tile(wk::xy(bbox$xmax, bbox$ymin, crs = crs), zoom)

    x <- seq(top_left$x, bottom_right$x)
    y <- seq(top_left$y, bottom_right$y)

    data.frame(
      x = rep(x, length(y)),
      y = rep(y, each = length(x)),
      zoom = zoom
    )
  } else if (crs_is_native(crs)) {
    bbox <- unclass(bbox)
    top_left <- osm_tile(wk::xy(bbox$xmin, bbox$ymax, crs = crs), zoom)
    bottom_right <- osm_tile(wk::xy(bbox$xmax, bbox$ymin, crs = crs), zoom)

    x <- seq(top_left$x, bottom_right$x)
    y <- seq(top_left$y, bottom_right$y)

    data.frame(
      x = rep(x, length(y)),
      y = rep(y, each = length(x)),
      zoom = zoom
    )
  } else {
    # Make a grid along the bounding box and compute the covering of that
    # after converting to native. This probably generates a covering that has
    # too many tiles in most cases.
    bbox_grd <- wk::grd(bbox, nx = 5, ny = 5, type = "corners")
    vertices_native <- osm_ensure_native(bbox_grd)
    osm_tile_covering(vertices_native, zoom = zoom)
  }
}

#' @rdname osm_tile_covering
#' @export
osm_zoom_num_tiles <- function(num_tiles) {
  force(num_tiles)
  function(bbox, result) {
    nrow(result) >= num_tiles
  }
}

#' Coordinate helpers
#'
#' @param x,y Ordinate values in EPSG:3857 (Spherical Mercator in meters)
#' @param lng,lat Coordinate values for longitude/latitude in degrees.
#' @inheritParams osm_tile
#'
#' @return
#'   - `osm_native()`, `osm_lnglat()`, `osm_ensure_native()`, and
#'     `osm_ensure_lnglat()` return a [wk::xy()] with the appropriate
#'     crs
#'   - `osm_crs_native()` returns a value that can be used as the [wk::wk_crs()]
#'     of a vector.
#' @export
#'
#' @examples
#' osm_lnglat(-64, 45)
#' osm_ensure_native(osm_lnglat(-64, 45))
#' osm_ensure_lnglat(
#'   osm_ensure_native(osm_lnglat(-64, 45))
#' )
osm_native <- function(x, y) {
  wk::xy(x, y, crs = osm_crs_native())
}

#' @rdname osm_native
#' @export
osm_lnglat <- function(lng, lat) {
  wk::xy(lng, lat, crs = wk::wk_crs_longlat())
}

#' @rdname osm_native
#' @export
osm_crs_native <- function() {
  "EPSG:3857"
}

#' @rdname osm_native
#' @export
osm_ensure_lnglat <- function(pt) {
  pt <- wk::as_xy(pt)
  crs <- wk::wk_crs(pt)
  if (is.null(crs)) {
    stop("Can't transform NULL crs to lon/lat")
  }

  if (inherits(crs, "wk_crs_inherit") || crs_is_lnglat(crs)) {
    wk::wk_set_crs(pt, wk::wk_crs_longlat())
  } else if (crs_is_native(crs)) {
    wk::xy(
      osm_native_to_lng_degrees(wk::xy_x(pt), scale = 6378137),
      osm_native_to_lat_degrees(wk::xy_y(pt), scale = 6378137),
      crs = wk::wk_crs_longlat()
    )
  } else {
    out <- sf::sf_project(
      wk::wk_crs_proj_definition(crs, verbose = TRUE),
      "EPSG:4326",
      as.matrix(pt),
      keep = TRUE,
      warn = FALSE,
      authority_compliant = FALSE
    )

    wk::new_wk_xy(
      list(
        x = out[, 1, drop = TRUE],
        y = out[, 2, drop = TRUE]
      ),
      crs = wk::wk_crs_longlat()
    )
  }
}

#' @rdname osm_native
#' @export
osm_ensure_native <- function(pt) {
  pt <- wk::as_xy(pt)
  crs <- wk::wk_crs(pt)
  if (is.null(crs)) {
    stop("Can't transform NULL crs to EPSG:3857")
  }

  if (inherits(crs, "wk_crs_inherit") || crs_is_native(crs)) {
    wk::wk_set_crs(pt, osm_crs_native())
  } else if (crs_is_lnglat(crs)) {
    wk::xy(
      osm_lng_degrees_to_native(wk::xy_x(pt), scale = 6378137),
      osm_lat_degrees_to_native(wk::xy_y(pt), scale = 6378137),
      crs = osm_crs_native()
    )
  } else {
    out <- sf::sf_project(
      wk::wk_crs_proj_definition(crs, verbose = TRUE),
      "EPSG:3857",
      as.matrix(pt),
      keep = TRUE,
      warn = FALSE,
      authority_compliant = FALSE
    )

    wk::new_wk_xy(
      list(
        x = out[, 1, drop = TRUE],
        y = out[, 2, drop = TRUE]
      ),
      crs = osm_crs_native()
    )
  }
}

crs_equal_any <- function(crs, dest_crs) {
  any(vapply(dest_crs, wk::wk_crs_equal, logical(1), crs))
}

crs_is_lnglat <- function(crs) {
  crs_equal_any(crs, list(wk::wk_crs_longlat(), "EPSG:4326"))
}

crs_is_native <- function(crs) {
  crs_equal_any(crs, list(osm_crs_native()))
}

ensure_tile <- function(tile) {
  if (!all(c("x", "y", "zoom") %in% names(tile))) {
    stop("`tile` must have columns `x`, `y`, and `zoom`")
  }

  if (!is.numeric(tile$x) || !is.numeric(tile$y) || !is.numeric(tile$zoom)) {
    stop("`tile$x`, `tile$y`, and `tile$zoom` must be numeric")
  }

  invisible(tile)
}

# Variants of projection functions whose projected range is [-pi -pi pi pi]
osm_lng_degrees_to_native <- function(lng_deg, scale = 1) {
  lng_deg / 180.0 * pi * scale
}

osm_lat_degrees_to_native <- function(lat_deg, scale = 1) {
  lat_rad <- lat_deg * pi / 180.0
  log(tan(lat_rad) + (1 / cos(lat_rad))) * scale
}

osm_native_to_lng_degrees <- function(x, scale = 1) {
  180.0 * x / pi / scale
}

osm_native_to_lat_degrees <- function(y, scale = 1) {
  atan(sinh(y / scale)) * 180 / pi
}
