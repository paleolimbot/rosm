
gdal_wms <- function(url_spec) {
   sprintf("<GDAL_WMS><Service name=\"TMS\"><ServerUrl>%s</ServerUrl></Service><DataWindow><UpperLeftX>-20037508.34</UpperLeftX><UpperLeftY>20037508.34</UpperLeftY><LowerRightX>20037508.34</LowerRightX><LowerRightY>-20037508.34</LowerRightY><TileLevel>18</TileLevel><TileCountX>1</TileCountX><TileCountY>1</TileCountY><YOrigin>top</YOrigin></DataWindow><Projection>EPSG:3857</Projection><BlockSizeX>256</BlockSizeX><BlockSizeY>256</BlockSizeY><BandsCount>3</BandsCount><!--<UserAgent>Please add a specific user agent text, to avoid the default one being used, and potentially blocked by OSM servers in case a too big usage of it would be seen</UserAgent>--><Cache /></GDAL_WMS>",
           url_spec)
}
#' Read directly from OSM tile sources via GDAL
#'
#' GDAL can read from tile sources without direct download, the 'target' argument
#' as a wk grd to specify the raster provided.
#'
#' A png source of tiles for OSM looks like 'https://tile.openstreetmap.org/${z}/${x}/${y}.png'.
#'
#' @param target wk grid specification (dimension, bbox, crs)
#' @param url_spec specification of OSM tile server (see Details)
#' @param
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
osm_warp <- function(target, url_spec, overwrite = FALSE) {


  bb <- as.numeric(wk::wk_bbox(target))
  crs <- wk::wk_crs(target)
  crsarg <- c("-t_srs", wk::wk_crs(target))
  if (is.na(crs) || nchar(crs) < 1) crsarg <- NULL
  out <- tempfile(fileext = ".tif")
  src <- gdal_wms(url_spec)
  if (overwrite) {
    overwritearg <- "-overwrite"
  } else {
    overwritearg <- NULL
  }
  opts <- c("-te", as.character(bb),
            crs,
            overwritearg,
            "-ts", as.character(dim(target_grd)[1:2]))
  sf::gdal_utils("warp", source = src, destination = out,  options = opts)
  out
}
