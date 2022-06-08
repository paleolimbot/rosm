
# function to extract the bounding box given an input object
# always returns the bbox in lat/lon

#' Extract a bounding box from an object
#'
#' This function is used internally by \link{osm.plot}, \link{bmaps.plot}, and
#' \link{osm.raster} to extract a bounding box from their first argument. This allows
#' considerable flexibility when specifying a location to map, in particular with
#' character input (a place name that will be geocoded), and other Spatial*/Raster*
#' objects.
#'
#' @param x A \code{Spatial*} object, a \code{Raster*} object, an sp bounding box,
#'   an sf bounding box,
#'   or a character string that will be passed to \code{searchbbox()} (prettymapr package). Multiple
#'   strings will result in a bounding box that contains all of the geocoded
#'   bounding boxes. The last resort is calling \code{sp::bbox()} on the \code{x}.
#' @param tolatlon Should the bounding box be un-projected to lat/lon coordinates?
#'   Only applied to Spatial and Raster objects.
#' @param ... Passed to \code{searchbbox()} if applicable
#'
#' @return A bounding box in the form of \code{sp::bbox()}
#' @export
#'
#' @examples
#' library(prettymapr)
#' ns <- makebbox(47.2, -59.7, 43.3, -66.4)
#' stopifnot(identical(ns, extract_bbox(ns)))
#'
extract_bbox <- function(x, tolatlon=TRUE, ...) {
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
  } else if(methods::is(x, "bbox")) {
    prettymapr::makebbox(x[4], x[3], x[2], x[1])
  } else if(is.character(x)) {
    # lookup using prettymapr::searchbbox()
    prettymapr::searchbbox(x, quiet = TRUE, ...)
  } else if(is.bbox(x)) {
    x
  } else {
    sp::bbox(x)
  }
}

is.bbox <- function(x) {
  is.matrix(x) && identical(dim(x), c(2L, 2L)) && identical(rownames(x), c("x", "y"))
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
