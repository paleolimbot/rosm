
#' Low-level tile math
#'
#' Open Street Map operates using a system of tiles whose value and bounds
#' are easily calculated from WGS84 longitude/latitude. These functions convert
#' between tile system coordinates and longitude/latitude.
#'
#' @param pt A vector of points as coerced by [wk::as_xy()]. The CRS for these
#'   points is considered.
#' @param lng,lat Coordinate values for longitude/latitude in degrees.
#' @param zoom A zoom level, generally between 0 and 21, with higher values
#'   representing a smaller (i.e., more detailed) tile.
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
osm_tile_top_left <- function(tile) {
  n <- 2.0^tile$zoom
  lon_deg <- tile$x / n * 360.0 - 180.0
  lat_rad <- atan(sinh(pi * (1 - 2 * tile$y / n)))
  lat_deg <- lat_rad * 180.0 / pi

  wk::xy(lon_deg, lat_deg, crs = wk::wk_crs_longlat())
}

#' @rdname osm_tile
#' @export
osm_tile_envelope <- function(tile) {
  top_left <- unclass(osm_tile_top_left(tile))
  bottom_right <- unclass(
    osm_tile_top_left(
      list(
        x = tile$x + 1,
        y = tile$y + 1,
        zoom = tile$zoom
      )
    )
  )

  wk::new_wk_rct(
    list(
      xmin = top_left$x,
      ymin = bottom_right$y,
      xmax = bottom_right$x,
      ymax = top_left$y
    ),
    crs = wk::wk_crs_longlat()
  )
}

#' @rdname osm_tile
#' @export
osm_native <- function(x, y) {
  wk::xy(x, y, crs = osm_crs_native())
}

#' @rdname osm_tile
#' @export
osm_lnglat <- function(lng, lat) {
  wk::xy(lng, lat, crs = wk::wk_crs_longlat())
}

#' @rdname osm_tile
#' @export
osm_crs_native <- function() {
  "EPSG:3857"
}

#' @rdname osm_tile
#' @export
osm_ensure_lnglat <- function(pt) {
  osm_ensure_internal(pt, list(wk::wk_crs_longlat(), "EPSG:4326"))
}

#' @rdname osm_tile
#' @export
osm_ensure_native <- function(pt) {
  osm_ensure_internal(pt, list(osm_crs_native()))
}

osm_ensure_internal <- function(pt, dest_crs) {
  pt <- wk::as_xy(pt)
  crs <- wk::wk_crs(pt)
  if (is.null(crs)) {
    stop("Can't transform NULL crs to lon/lat")
  }

  crs_equal <- vapply(dest_crs, wk::wk_crs_equal, logical(1), crs)

  if (inherits(crs, "wk_crs_inherit")) {
    wk::wk_set_crs(pt, dest_crs[[1]])
  } else if (!any(crs_equal)) {
    out <- sf::sf_project(
      wk::wk_crs_proj_definition(crs, verbose = TRUE),
      dest_crs[[1]],
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
      crs = dest_crs[[1]]
    )
  } else {
    pt
  }
}

ensure_tile <- function(tile) {
  if (!all(c("x", "y", "zoom") %in% names(tile))) {
    stop("`tile` must have columns `x`, `y`, and `zoom`")
  }

  if (!is.numeric(tile$x) || !is.numeric(tile$y) || !is.numeric(tile$zoom)) {
    stop("`tile$x`, `tile$y`, and `tile$zoom` must be numeric")
  }
}

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
  2 * asin(y / pi / scale)
}
