.onLoad <- function(libname, pkgname) {
  gdwcp <- Sys.getenv("GDAL_DEFAULT_WMS_CACHE_PATH")
  if (is.null(gdwcp) || nchar(gdwcp) < 1) {

  }
  ## something like this ...
  Sys.setenv(GDAL_DEFAULT_WMS_CACHE_PATH = tile.cachedir(list(name = "gdalwmscache")))
}


gdal_wms <- function(url_spec, type = "tms") {


  ##               osm: https://tile.openstreetmap.org/${z}/${x}/${y}.png
  ## bing/virtualearth: http://a${server_num}.ortho.tiles.virtualearth.net/tiles/a${quadkey}.jpeg?g=90
   switch(type,

          tms = sprintf("<GDAL_WMS><Service name=\"TMS\"><ServerUrl>%s</ServerUrl></Service><DataWindow><UpperLeftX>-20037508.34</UpperLeftX><UpperLeftY>20037508.34</UpperLeftY><LowerRightX>20037508.34</LowerRightX><LowerRightY>-20037508.34</LowerRightY><TileLevel>18</TileLevel><TileCountX>1</TileCountX><TileCountY>1</TileCountY><YOrigin>top</YOrigin></DataWindow><Projection>EPSG:3857</Projection><BlockSizeX>256</BlockSizeX><BlockSizeY>256</BlockSizeY><BandsCount>3</BandsCount><!--<UserAgent>Please add a specific user agent text, to avoid the default one being used, and potentially blocked by OSM servers in case a too big usage of it would be seen</UserAgent>--><Cache /></GDAL_WMS>",
           url_spec),
          virtualearth = sprintf("<GDAL_WMS><Service name=\"VirtualEarth\"><ServerUrl>%s</ServerUrl></Service><MaxConnections>4</MaxConnections><Cache/></GDAL_WMS>",
                         url_spec) )

}
#' Read directly from OSM tile sources via GDAL
#'
#' GDAL can read from tile sources without direct download, the 'target' argument
#' as a wk grd to specify the raster pr
#' ovided.
#'
#' A png source of tiles for OSM or general TMS looks like 'https://tile.openstreetmap.org/${z}/${x}/${y}.png'.
#'
#' VirtualEarth tile source is of type "tms".
#'
#' A jpeg source of ortho tiles for VirtualEarth looks like http://a${server_num}.ortho.tiles.virtualearth.net/tiles/a${quadkey}.jpeg?g=90
#'
#' A jpeg source of streetmap tiles for VirtualEarth looks like http://r${server_num}.ortho.tiles.virtualearth.net/tiles/r${quadkey}.jpeg?g=90
#'
#' @param target wk grid specification (dimension, bbox, crs)
#' @param resample resampling algorithm (corresponds to '-r' in gdalwarp utility)
#' @param url_spec specification of OSM tile server (see Details)
#' @param type "tms" for generic tile map server (like OSM), or "virtualearth" for the specific Bing maps quadkey (see Details)
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
#' ## altalake in Mercator
#' grd2 <- wk::grd(wk::rct(-13693753, 6464083, -13686567, 6467776, crs = "EPSG:3857"),
#'  nx = 1024, ny = 512)   #size should account for aspect ratio and match the device targeted
#' tif2 <- osm_warp(grd2, url_spec)
#'
#' u3 <- "http://r${server_num}.ortho.tiles.virtualearth.net/tiles/r${quadkey}.jpeg?g=90"
#' tif3 <- osm_warp(grd2, u3, type = "virtualearth")
#'
#' u4 <- "http://r${server_num}.ortho.tiles.virtualearth.net/tiles/a${quadkey}.jpeg?g=90"
#' tif4 <- osm_warp(grd2, u4, type = "virtualearth")
#'
#' ## now we can unpack the file to get to the right orientation and colour type
#' d <- attr(sf::gdal_read(tif4, options = c("-ot", "Byte")), "data")
#' cl <- matrix(rgb(d[,,1], d[,,2], d[,,3], maxColorValue= 255), dim(d)[2], byrow = TRUE)
#' grd2$data <- cl
#' plot(grd2)
#' ## more examples: https://gist.github.com/mdsumner/91f3d00d707ce9ea25c7d70a68ec53c0
osm_warp <- function(target, url_spec, resample = "near", type = "tms") {
  bb <- as.numeric(wk::wk_bbox(target))
  crs <- wk::wk_crs(target)
  crsarg <- c("-t_srs", wk::wk_crs(target))
  if (is.na(crs) || nchar(crs) < 1) crsarg <- NULL
  out <- tempfile(fileext = ".tif")
  src <- gdal_wms(url_spec, type = type)

  opts <- c("-te", as.character(bb),
            crsarg,
            "-ts", as.character(dim(target)[1:2]),
            "-r", resample)
  ## WIP we could set GDAL_DEFAULT_WMS_CACHE_PATH
  ## this otherwise results in a gdalwmscache/ folder in curdir
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



