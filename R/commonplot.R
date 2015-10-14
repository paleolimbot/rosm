#functions used by both google and osm

tile.cachedir <- function(type, cachedir=NULL) {
  if(is.null(cachedir)) {
    cachedir <- "rosm.cache"
  }
  folder <- file.path(cachedir, type)
  created <- dir.create(folder, showWarnings=FALSE, recursive=TRUE)
  folder
}

tile.plotarray <- function(image, box) {
  rasterImage(image, box[1,1], box[2,1], box[1,2], box[2,2])
}

tile.autozoom <- function(res=150, epsg=4326) {
  ext <- par("usr")
  midy <- mean(c(ext[3], ext[4]))
  rightmid <- .tolatlon(ext[2], midy, epsg)
  leftmid <- .tolatlon(ext[1], midy, epsg)
  anglewidth <- rightmid[1] - leftmid[1]
  if(anglewidth < 0) {
    anglewidth <- anglewidth+360
  }
  #PROBLEMS WITH WIDE EXTENTS LIKE THE WORLD
  widthin <- graphics::grconvertX(ext[2], from="user", to="inches") -
    graphics::grconvertX(ext[1], from="user", to="inches")
  widthpx <- widthin * res

  zoom = log2((360.0 / anglewidth) * (widthpx / 256.0))

  as.integer(floor(zoom))
}
