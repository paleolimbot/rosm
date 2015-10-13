#google static API plotting

google.getimage <- function(maptype, lon, lat, zoom, wdpx,
                            htpx, scale=1, key=NULL, cachedir=NULL, forcedownload=FALSE) {
  if(is.null(key)) {
    key <- "AIzaSyA7TenP6BraUbiUvea_bhJqdeaK1nyOSjE"
  }
  format="png"

  url <- paste0("https://maps.googleapis.com/maps/api/staticmap?maptype=", "satellite",
                "&center=", lat, ",", lon, "&zoom=", zoom,
                "&size=", wdpx, "x", htpx, "&key=", key, "&format=", format,
                "&scale=", scale)
  folder <- tile.cachedir("google", cachedir)
  filename <- paste0(digest::digest(url), ".", format)
  tofile <- file.path(folder, filename)

  if(!file.exists(tofile) || forcedownload) {
    message("Downloading to ", tofile)
    download.file(url, tofile, quiet=TRUE)
  }

  png::readPNG(tofile)
}



#' Plot Google Maps
#'
#' Plot google maps.
#'
#' @param bbox
#' @param zoomin
#' @param zoom
#' @param maptype
#' @param forcedownload
#' @param cachedir
#' @param res
#' @param epsg
#' @param key
#' @param ...
#'
#' @export
#'

gmap.plot <- function(bbox, maptype="satellite", forcedownload=FALSE,
                           cachedir=NULL, res=150, project=TRUE, key=NULL, ...) {

  if(project) {
    epsg <- 3857
  } else {
    epsg <- 4326
  }
  #setup plot
  bboxplot <- .projectbbox(bbox, epsg)
  coords <- sp::coordinates(t(bboxplot))
  spoints = sp::SpatialPoints(coords, proj4string = sp::CRS(paste0("+init=epsg:", epsg)))
  plotargs <- list(...)
  if(is.null(plotargs$xlim))
    xlim <- bboxplot[1,]
  if(is.null(plotargs$ylim))
    ylim <- bboxplot[2,]
  sp::plot(spoints, pch=".", xlim=xlim, ylim=ylim, ...)

  #get extents
  ext <- par("usr")
  fullareabbox <- matrix(c(ext[1], ext[3], ext[2], ext[4]), ncol=2, byrow=FALSE)
  fullareabboxll <- .revprojectbbox(fullareabbox, epsg)

  bboxgoog <- .projectbbox(fullareabboxll, 3857)
  if((bboxgoog[1,2] - bboxgoog[1,1]) < 0) {
    bboxgoog[1,2] <- bboxgoog[1,2]+ 20037508*2
  }
  midx <- (bboxgoog[1]+bboxgoog[3])/2
  midy <- (bboxgoog[2]+bboxgoog[4])/2
  midlatlon <- .tolatlon(midx, midy, 3857)

  zoom <- tile.autozoom(res, epsg)
  #y / x
  aspect <- (bboxgoog[2,2] - bboxgoog[2,1]) / (bboxgoog[1,2] - bboxgoog[1,1])
  if(aspect < 0) {
    aspect <- (bboxgoog[2,2] - bboxgoog[2,1]) / (bboxgoog[1,2] - bboxgoog[1,1])
  }


  anglewidth <- fullareabboxll[1,2] - fullareabboxll[1,1]
  if(anglewidth < 0) {
    anglewidth <- anglewidth+360
  }
  totpixels <- 2^zoom*256
  widthpx <- (anglewidth / 360.0) * totpixels
  heightpx <- widthpx * aspect

  #calc scale factor based on res parameter
  widthin <- graphics::grconvertX(ext[2], from="user", to="inches") -
    graphics::grconvertX(ext[1], from="user", to="inches")
  actualres <- widthpx / widthin
  if(actualres < res) {
    scale <- 2
  } else {
    scale <- 1
  }

  image <- google.getimage(maptype, midlatlon[1], midlatlon[2], zoom, round(widthpx),
                           round(heightpx), scale=scale, key=key, cachedir=cachedir,
                           forcedownload=forcedownload)
  tile.plotarray(image, fullareabbox)
}
