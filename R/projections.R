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
