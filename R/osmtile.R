
#modified from http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#R


tiles.bybbox <- function(bbox, zoom, epsg=4326) {
  nwlatlon <- .tolatlon(bbox[1,1], bbox[2,2], epsg)
  selatlon <- .tolatlon(bbox[1,2], bbox[2,1], epsg)

  if(nwlatlon[1] > selatlon[1]) {
    #wrapping around backside of earth
    backsidebbox <- matrix(c(nwlatlon[1], selatlon[2], 180, nwlatlon[2]), ncol=2, byrow=FALSE)
    backsidetiles <- tiles.bybbox(backsidebbox, zoom, 4326)
    nwlatlon[1] <- -180
  } else {
    backsidetiles <- NULL
  }

  nw <- tile.xy(nwlatlon[1], nwlatlon[2], zoom)
  se <- tile.xy(selatlon[1], selatlon[2], zoom)
  tiles <- expand.grid(nw[1]:se[1], nw[2]:se[2])

  if(is.null(backsidetiles)) {
    tiles
  } else {
    rbind(backsidetiles, tiles)
  }
}

tile.xy <-function(x, y, zoom, epsg=4326) {
  latlon <- .tolatlon(x, y, epsg)
  if(latlon[1] >= 180) {
    latlon[1] <- 179.9999
  } else if(latlon[1] < -180) {
    latlon[1] <- -180
  }
  if(latlon[2] > 85.0511) {
    latlon[2] <- 85.0511
  } else if(latlon[2] <= -85.0511) {
    latlon[2] <- -85.05109
  }

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

  projected <- .fromlatlon(lon_deg, lat_deg, epsg)
  #added to support wrap around tiles
  if(lon_deg < -180 && projected[1] > 0) {
    #wrap around situation

    maxx <- .fromlatlon(180, lat_deg, epsg)[1]
    projected[1] <- projected[1]-maxx*2
  }
  projected
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

tile.ext <- function(type) {
  parts <- strsplit(tile.url(0,0,0, type), "\\.")[[1]]
  parts2 <- strsplit(parts[length(parts)], "\\?")[[1]]
  parts2[1]
}

tile.maxzoom <- function(type) {
  if(existsFunction(paste0("tile.maxzoom.", type))) {
    do.call(paste0("tile.maxzoom.", type), list())
  } else {
    return(19)
  }
}

tile.cachename <- function(xtile, ytile, zoom, type, cachedir=NULL) {
  folder <- tile.cachedir(type, cachedir)
  ext <- tile.ext(type)
  file.path(folder, paste0(zoom, "_", xtile, "_", ytile, ".", ext))
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
