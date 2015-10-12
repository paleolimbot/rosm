#projection functions

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

.projectbbox <- function(bbox, toepsg) {
  coords <- sp::coordinates(t(bbox))
  spoints = sp::SpatialPoints(coords, proj4string = sp::CRS("+init=epsg:4326"))
  newpoints <- sp::spTransform(spoints, sp::CRS(paste0("+init=epsg:", toepsg)))
  t(sp::coordinates(newpoints))
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
