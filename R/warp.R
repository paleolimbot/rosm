
gdal_wms <- function(url_spec) {
   sprintf("<GDAL_WMS><Service name=\"TMS\"><ServerUrl>%s</ServerUrl></Service><DataWindow><UpperLeftX>-20037508.34</UpperLeftX><UpperLeftY>20037508.34</UpperLeftY><LowerRightX>20037508.34</LowerRightX><LowerRightY>-20037508.34</LowerRightY><TileLevel>18</TileLevel><TileCountX>1</TileCountX><TileCountY>1</TileCountY><YOrigin>top</YOrigin></DataWindow><Projection>EPSG:3857</Projection><BlockSizeX>256</BlockSizeX><BlockSizeY>256</BlockSizeY><BandsCount>3</BandsCount><!--<UserAgent>Please add a specific user agent text, to avoid the default one being used, and potentially blocked by OSM servers in case a too big usage of it would be seen</UserAgent>--><Cache /></GDAL_WMS>",
           url_spec)
}
#' Read directly from OSM tile sources via GDAL
#'
#' GDAL can read from tile sources without direct download, the 'target' argument
#' as a wk grd to specify the raster pr
#' ovided.
#'
#' A png source of tiles for OSM looks like 'https://tile.openstreetmap.org/${z}/${x}/${y}.png'.
#'
#' @param target wk grid specification (dimension, bbox, crs)
#' @param resample resampling algorithm (corresponds to '-r' in gdalwarp utility)
#' @param url_spec specification of OSM tile server (see Details)

#' @return file path to resulting GeoTIFF
#' @export
#'
#' @examples
#' bounds <- wk::rct(
#'  252185, 4815826, 739729, 5210280,
#'  crs = "EPSG:32620"
#' )
#'
#' target_grd <- wk::grd(bounds, dx = 500, dy = 500)
#' url_spec <- "https://tile.openstreetmap.org/${z}/${x}/${y}.png"
#' tifpath <- osm_warp(target_grd, url_spec)
#'
#' grd1 <- wk::grd(wk::rct(-5e6, -5e6, 5e6, 5e6, crs = "+proj=laea +lon_0=180"),
#'                  nx = 1024, ny = 1024)
#' tif1 <- osm_warp(grd1, url_spec)
#'
#' ## more examples: https://gist.github.com/mdsumner/91f3d00d707ce9ea25c7d70a68ec53c0
osm_warp <- function(target, url_spec, resample = "near") {
  bb <- as.numeric(wk::wk_bbox(target))
  crs <- wk::wk_crs(target)
  crsarg <- c("-t_srs", wk::wk_crs(target))
  if (is.na(crs) || nchar(crs) < 1) crsarg <- NULL
  out <- tempfile(fileext = ".tif")
  src <- gdal_wms(url_spec)

  opts <- c("-te", as.character(bb),
            crsarg,
            "-ts", as.character(dim(target)[1:2]),
            "-r", resample)
  res <- sf::gdal_utils("warp", source = src, destination = out,  options = opts)
  if (!res) stop("gdalwarp app lib call failed")
  out
}



## reference example https://gist.github.com/mdsumner/91f3d00d707ce9ea25c7d70a68ec53c0?permalink_comment_id=4429675#gistcomment-4429675
# f <- whatarelief:::.imagery_sources[1]
# info <- vapour::vapour_raster_info(f)
# ex <- c(-1, 1, -1, 1) * 5e6
# crs <- "+proj=laea +lon_0=180"
# sf::gdal_utils("warp", source = f, destination = tif <- tempfile(fileext = ".tif"),
#                                                                  options = c("-overwrite",
#                                                                  			"-ts", c(512, 512),
#                                                                  			"-t_srs", crs,
#                                                                              "-te", as.character(ex[c(1, 3, 2, 4)])))
# library(terra)
# plot(rast(tif))
#



