# projection functions

.tolatlon <- function(x, y, epsg=NULL, projection=NULL) {
  requireNamespace("rgdal", quietly=TRUE)

  if(is.null(epsg) && is.null(projection)) {
    stop("epsg and projection both null...nothing to project")
  } else if(!is.null(epsg) && !is.null(projection)) {
    stop("epsg and projection both specified...ambiguous call")
  }

  if(is.null(projection)) {
    projection <- sp::CRS(paste0("+init=epsg:", epsg))
  }

  coords <- sp::coordinates(cbind(x, y))
  rownames(coords) <- NULL
  spoints <- sp::SpatialPoints(coords, projection)
  spnew <- sp::spTransform(spoints, sp::CRS("+init=epsg:4326"))
  sp::coordinates(spnew)
}

.fromlatlon <- function(lon, lat, epsg=NULL, projection=NULL) {
  requireNamespace("rgdal", quietly=TRUE)

  if(is.null(epsg) && is.null(projection)) {
    stop("epsg and projection both null...nothing to project")
  } else if(!is.null(epsg) && !is.null(projection)) {
    stop("epsg and projection both specified...ambiguous call")
  }

  if(is.null(projection)) {
    projection <- sp::CRS(paste0("+init=epsg:", epsg))
  }

  coords <- sp::coordinates(cbind(lon, lat))
  rownames(coords) <- NULL
  spoints <- sp::SpatialPoints(coords, sp::CRS("+init=epsg:4326"))
  spnew <- sp::spTransform(spoints, projection)
  sp::coordinates(spnew)
}

.projectbbox <- function(bbox, toepsg=NULL, projection=NULL) {
  requireNamespace("rgdal", quietly=TRUE)

  if(is.null(toepsg) && is.null(projection)) {
    stop("toepsg and projection both null...nothing to project")
  } else if(!is.null(toepsg) && !is.null(projection)) {
    stop("toepsg and projection both specified...ambiguous call")
  }

  if(is.null(projection)) {
    projection <- sp::CRS(paste0("+init=epsg:", toepsg))
  }
  coords <- sp::coordinates(t(bbox))
  spoints = sp::SpatialPoints(coords, proj4string = sp::CRS("+init=epsg:4326"))
  newpoints <- sp::spTransform(spoints, projection)
  newbbox <- t(sp::coordinates(newpoints))

  if(newbbox[1,1] > newbbox[1,2]) { #if min>max
    maxx <- .fromlatlon(180, bbox[2, 1], projection=projection)[1]
    newbbox[1,1] <- newbbox[1,1]-maxx*2
  }
  newbbox
}

.revprojectbbox <- function(bbox, fromepsg=NULL, projection=NULL) {
  requireNamespace("rgdal", quietly=TRUE)

  if(is.null(fromepsg) && is.null(projection)) {
    stop("fromepsg and projection both null...nothing to project")
  } else if(!is.null(fromepsg) && !is.null(projection)) {
    stop("fromepsg and projection both specified...ambiguous call")
  }
  if(is.null(projection)) {
    projection <- sp::CRS(paste0("+init=epsg:", fromepsg))
  }
  coords <- sp::coordinates(t(bbox))
  spoints = sp::SpatialPoints(coords, proj4string = projection)
  newpoints <- sp::spTransform(spoints, sp::CRS("+init=epsg:4326"))
  t(sp::coordinates(newpoints))
}
