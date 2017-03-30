
# function to extract the bounding box given an input object
# always returns the bbox in lat/lon
extract_bbox <- function(x, tolatlon=TRUE) {
  if(methods::is(x, "Spatial")) {
    box <- sp::bbox(x)
    if(tolatlon && !is.na(rgdal::CRSargs(x@proj4string))) {
      requireNamespace("rgdal", quietly = TRUE)
      box <- sp::bbox(sp::spTransform(x, sp::CRS("+init=epsg:4326")))
    }
    box
  } else if(methods::is(x, "Raster")) {
    box <- raster::as.matrix(x@extent)
    if(tolatlon && !is.na(rgdal::CRSargs(x@crs))) {
      requireNamespace("rgdal", quietly = TRUE)

      # need a couple of points to get a decent approximation
      coords <- expand.grid(x=box[1,], y=box[2,])
      box <- sp::bbox(.tolatlon(coords[, 1], coords[, 2], projection = x@crs))
    }
    box
  } else if("character" %in% class(x)) {
    # should probably look up
    stop("Character lookups not yet implemented")
  } else if(is.matrix(x) && identical(dim(x), c(2L, 2L))) {
    x
  } else {
    stop("Don't know how to guess a bounding box from type ", class(x))
  }
}

# function to extract a projection from an object
extract_projection <- function(x) {
  if(methods::is(x, "CRS")) {
    x
  } else if(methods::is(x, "Spatial")) {
    if(!is.na(rgdal::CRSargs(x@proj4string))) {
      x@proj4string
    } else {
      NA
    }
  } else if(methods::is(x, "Raster")) {
    x@crs
  } else if(is.numeric(x) && (length(x) == 1)) {
    requireNamespace("rgdal", quietly=TRUE)
    intx <- as.integer(x)
    sp::CRS(paste0("+init=epsg:", intx))
  } else {
    NA
  }
}
