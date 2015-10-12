
#modified from http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#R

.tolatlon <- function(x, y, epsg) {
  rgdal::CRSargs(sp::CRS(paste0("+init=epsg:", epsg))) #hack to make sure rgdal stays in Imports:
  coords <- sp::coordinates(matrix(c(x,y), byrow=TRUE, ncol=2))
  spoints <- sp::SpatialPoints(coords, sp::CRS(paste0("+init=epsg:", epsg)))
  spnew <- sp::spTransform(spoints, sp::CRS("+init=epsg:4326"))
  c(sp::coordinates(spnew)[1], sp::coordinates(spnew)[2])
}

.fromlatlon <- function(lon, lat, epsg) {
  coords <- sp::coordinates(matrix(c(lon,lat), byrow=TRUE, ncol=2))
  spoints <- sp::SpatialPoints(coords, sp::CRS("+init=epsg:4326"))
  spnew <- sp::spTransform(spoints, sp::CRS(paste0("+init=epsg:", epsg)))
  c(sp::coordinates(spnew)[1], sp::coordinates(spnew)[2])
}

.geodist <- function(x1, y1, x2, y2, epsg) {
  lonlat1 <- .tolatlon(x1, y1, epsg)
  lonlat2 <- .tolatlon(x2, y2, epsg)

  long1 <- .torad(lonlat1[1])
  lat1 <- .torad(lonlat1[2])
  long2 <- .torad(lonlat2[1])
  lat2 <- .torad(lonlat2[2])
  R <- 6371009 # Earth mean radius [m]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in m
}

tile.cachedir <- function(type, cachedir=NULL) {
  if(is.null(cachedir)) {
    cachedir <- "rosm.cache"
  }
  folder <- file.path(cachedir, type)
  created <- dir.create(folder, showWarnings=FALSE, recursive=TRUE)
  folder
}

tiles.bybbox <- function(bbox, zoom, epsg=4326) {
  nwlatlon <- .tolatlon(bbox[1,1], bbox[2,2], epsg)
  selatlon <- .tolatlon(bbox[1,2], bbox[2,1], epsg)

  nw <- tile.xy(nwlatlon[1], nwlatlon[2], zoom)
  se <- tile.xy(selatlon[1], selatlon[2], zoom)
  expand.grid(nw[1]:se[1], nw[2]:se[2])
}

tile.autozoom <- function(res=150, epsg=4326) {
  ext <- par("usr")
  midy <- mean(c(ext[3], ext[4]))
  rightmid <- .tolatlon(ext[2], midy, epsg)
  leftmid <- .tolatlon(ext[1], midy, epsg)
  anglewidth <- rightmid[1] - leftmid[1]

  widthin <- graphics::grconvertX(ext[2], from="user", to="inches") -
    graphics::grconvertX(ext[1], from="user", to="inches")
  widthpx <- widthin * res

  zoom = log2((360.0 / anglewidth) * (widthpx / 256.0))
  as.integer(floor(zoom))
}

tile.xy <-function(x, y, zoom, epsg=4326) {
  latlon <- .tolatlon(x, y, epsg)
  lat_rad <- latlon[2] * pi /180
  n <- 2.0 ^ zoom
  xtile <- floor((latlon[1] + 180.0) / 360.0 * n)
  ytile = floor((1.0 - log(tan(lat_rad) + (1 / cos(lat_rad))) / pi) / 2.0 * n)
  c(xtile, ytile)
}

tile.nw <- function(xtile, ytile, zoom, epsg=4326) {
  n <- 2.0 ^ zoom
  lon_deg <- xtile / n * 360.0 - 180.0
  lat_rad <- atan(sinh(pi * (1 - 2 * ytile / n)))
  lat_deg <- lat_rad*180/pi

  .fromlatlon(lon_deg, lat_deg, epsg)
}

tile.bbox <- function(xtile, ytile, zoom, epsg=4326) {
  nw <- tile.nw(xtile, ytile, zoom, epsg)
  se <- tile.nw(xtile+1, ytile+1, zoom, epsg)
  matrix(c(nw[1], se[2], se[1], nw[2]), ncol=2,
         byrow=FALSE, dimnames=list(c("x", "y"), c("min", "max")))
}

tile.url <- function(xtile, ytile, zoom, type) {
  do.call(paste0("tile.url.", type), list(xtile, ytile, zoom))
}

tile.maxzoom <- function(type) {
  if(existsFunction(paste0("tile.maxzoom.", type))) {
    do.call(paste0("tile.maxzoom.", type), list())
  } else {
    return(20)
  }
}

tile.cachename <- function(xtile, ytile, zoom, type, cachedir=NULL) {
  folder <- tile.cachedir(type, cachedir)
  ext <- ".png" #maybe only for osm
  file.path(folder, paste0(zoom, "_", xtile, "_", ytile, ext))
}

tile.download <- function(tiles, zoom, type="osm", forcedownload=FALSE, cachedir=NULL) {
  for(i in 1:nrow(tiles)) {
    xtile <- tiles[i,1]
    ytile <- tiles[i,2]
    cachename <- tile.cachename(xtile, ytile, zoom, type, cachedir)
    if(!file.exists(cachename) || forcedownload) {
      url <- tile.url(xtile, ytile, zoom, type)
      message("Downloading from ", url)
      tryCatch(download.file(url, cachename, quiet = TRUE),
               error=function(err) {
                 message("Error downloading tile ", xtile, ",", ytile, " (zoom: ",
                         zoom, "): ", err)
               })
    }
  }
}
