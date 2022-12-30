
osm_lnglat <- function(lng, lat) {
  wk::xy(lng, lat, crs = wk::wk_crs_longlat())
}

osm_tile <- function(pt, zoom) {
  lnglat <- unclass(ensure_lnglat(pt))

  lat_rad <- lnglat[[2]] * pi / 180.0
  n <- 2.0^zoom
  xtile <- floor((lnglat[[1]] + 180.0) / 360.0 * n)
  ytile <- floor((1.0 - log(tan(lat_rad) + (1.0 / cos(lat_rad))) / pi) / 2.0 * n)
  data.frame(x = xtile, y = ytile, zoom = zoom)
}

osm_tile_top_left <- function(tile) {
  n <- 2.0^tile$zoom
  lon_deg <- tile$x / n * 360.0 - 180.0
  lat_rad <- atan(sinh(pi * (1 - 2 * tile$y / n)))
  lat_deg <- lat_rad * 180.0 / pi

  wk::xy(lon_deg, lat_deg, crs = wk::wk_crs_longlat())
}

osm_tile_envelope <- function(tile) {
  top_left <- unclass(osm_tile_top_left(tile))
  bottom_right <- unclass(osm_tile_top_left(list(x = tile$x + 1, y = tile$y + 1, zoom = tile$zoom)))

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

ensure_lnglat <- function(pt) {
  pt <- wk::as_xy(pt)
  crs <- wk::wk_crs(pt)
  if (is.null(crs)) {
    stop("Can't transform NULL crs to lon/lat")
  }

  if (!wk::wk_crs_equal(crs, wk::wk_crs_longlat()) &&
      !wk::wk_crs_equal(crs, "EPSG:4326")) {
    out <- sf::sf_project(
      wk::wk_crs_proj_definition(crs, verbose = TRUE),
      "EPSG:4326",
      as.matrix(pt),
      keep = TRUE,
      warn = FALSE,
      authority_compliant = FALSE
    )

    pt <- wk::new_wk_xy(
      list(
        x = out[, 1, drop = TRUE],
        y = out[, 2, drop = TRUE]
      ),
      crs = wk::wk_crs_longlat()
    )
  }

  pt
}
