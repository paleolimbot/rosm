# plot slippy tiles

tile.loadimage <- function(x, y, zoom, type, cachedir=NULL, quiet = TRUE) {
  if(x < 0) {
    # negative tiles from wrap situation
    x <- x+2^zoom
  }

  fname <- tile.cachename(x, y, zoom, type, cachedir)
  parts <- strsplit(fname, "\\.")[[1]]
  ext <- parts[length(parts)]
  tryCatch({
    if(ext == "jpg" || ext =="jpeg") {
      jpeg::readJPEG(fname)
    } else if(ext == "png") {
      png::readPNG(fname)
    } else {
      stop("Extension not recognized: ", ext)
    }
  }, error=function(err) {
    if(!quiet) message("Error loading ", fname, ": ", err)
    NULL
  })
}

tile.applywrap <- function(tiles, zoom) {
  if(!all(min(tiles[,1]):max(tiles[,1]) %in% tiles[,1])) {
    # wrapping around the backside of the earth, make end tiles negative
    warning("Attempting to plot wrap around tiles (~lat=-180), things may get funky.")
    n <- -1
    while(length(tiles[,1][tiles[,1]==2^zoom+n]) > 0) {
      tiles[,1][tiles[,1]==2^zoom+n] <- (tiles[,1][tiles[,1]==2^zoom+n]) - 2^zoom
      n <- n-1
    }
  }
  tiles
}

# loops through the tiles applies a function (returning a list)
tile.apply <- function(tiles, zoom, type, fun, epsg=4326, cachedir=NULL, ...,
                       progress = "none") {
  plyr::alply(tiles, 1, function(tile) {
    x <- tile[[1]]
    y <- tile[[2]]
    fun(x, y, zoom=zoom, type=type, epsg=epsg, cachedir=cachedir, ...)
  }, .progress = progress)
}

# loops through the tiles and plots or combines the results to a list
tile.ploteach <- function(tiles, zoom, type, epsg=4326, cachedir=NULL, quiet = FALSE) {
  tile.apply(tiles, zoom, type, function(x, y, zoom, type, epsg, cachedir) {
    box <- tile.bbox(x, y, zoom, epsg)
    image <- tile.loadimage(x, y, zoom, type, cachedir, quiet = quiet)

    # if in plotting mode, plot the array
    if(!is.null(image)) tile.plotarray(image, box)

  }, epsg=epsg, cachedir=cachedir)
}

tile.each <- function(tiles, zoom, type, epsg=4326, cachedir=NULL, quiet = FALSE) {
  tile.apply(tiles, zoom, type, function(x, y, zoom, type, epsg, cachedir) {
    box <- tile.bbox(x, y, zoom, epsg)
    image <- tile.loadimage(x, y, zoom, type, cachedir, quiet = quiet)

    # return structure as the image array, with attribute 'bbox'
    # this is modeled after the @bbox slot in the sp package
    structure(image, bbox=box, type=type,
              epsg=epsg, zoom=zoom, tiles = data.frame(x, y))
  })
}

# shortcut to abind(..., along=1)
tile.arbind <- function(...) {
  abind::abind(..., along=1)
}

# shortcut to abind(..., along=2)
tile.acbind <- function(...) {
  abind::abind(..., along=2)
}

tile.fuse <- function(tiles, zoom, type, epsg=4326, cachedir=NULL, quiet = FALSE) {

  tiles <- tile.applywrap(tiles, zoom)

  tile_dims <- check.dimensions(tiles, zoom, type, epsg, cachedir)

  if(tile_dims$nmissing > 0) {
    message(tile_dims$nmissing, " could not be loaded for type ", type$name)
    missing_tile <- array(0, tile_dims$targetdim)
  } else {
    missing_tile <- NULL
  }

  tiles <- tiles[order(tiles$Var1, tiles$Var2),]
  xs <- unique(tiles[,1])
  ys <- unique(tiles[,2])

  # bind all the tiles together. foreach was slightly faster
  # but this is far simpler, and doesn't invoke another dependency
  wholeimg <- do.call(tile.acbind, lapply(xs, function(x) {
    do.call(tile.arbind, lapply(ys, function(y) {
      img <- tile.loadimage(x, y, zoom, type, cachedir, quiet = quiet)
      if(is.null(img) && is.null(missing_tile)) {
        stop("Cannot fuse unloadable tile")
      } else if(is.null(img)) {
        missing_tile
      } else {
        ensure.bands(img, tile_dims$targetdim, default_value = 1)
      }
    }))
  }))

  # calc bounding box of whole image
  nw <- tile.nw(min(xs), min(ys), zoom, epsg)
  se <- tile.nw(max(xs)+1, max(ys)+1, zoom, epsg)

  bbox <- matrix(c(nw[1], se[2], se[1], nw[2]), ncol=2,
                byrow=FALSE, dimnames=list(c("x", "y"), c("min", "max")))

  # return same structure as tile.each()
  structure(wholeimg, bbox=bbox, epsg=epsg,
            type=type, zoom=zoom, tiles = tiles)
}

# ensure array dimensions match a given dim value
ensure.bands <- function(image, dimension, default_value=1) {
  banddiff <- dimension[3] - dim(image)[3]
  if(banddiff == 0) {
    image
  } else if (banddiff > 0) {
    # add extra bands
    abind::abind(image, array(default_value,
                              c(dimension[1], dimension[2], banddiff)),
                 along = 3)
  } else if(banddiff < 0) {
    # this shouldn't happen, but...
    warning("Cropping image in ensure.bands")
    # crop
    image[ , , 1:dimension[3], drop = FALSE]
  }
}


# checks the dimensions of all the tiles (used in tile.fuse)
check.dimensions <- function(tiles, zoom, type, epsg, cachedir) {
  # check dimensions of all tiles before fusing
  dims <- tile.apply(tiles, zoom, type, fun=function(x, y, zoom, type, epsg, cachedir) {
    image <- tile.loadimage(x, y, zoom, type, cachedir)
    if(!is.null(image)) {
      dim(image)
    } else {
      c(0, 0, 0)
    }
  })

  # check for 3 dimensions
  if(!all(vapply(dims, length, integer(1)) == 3)) stop("Incorrect dimensions in image")

  # check for missing tiles
  missing_tiles <- vapply(dims, function(dim) identical(dim, c(0, 0, 0)),
                          logical(1))
  if(all(missing_tiles)) stop("Zero tiles were loaded for type ", type$name)

  # find dimension of non-missing tiles (hopefully the same...)
  tiledim <- do.call(rbind, dims[!missing_tiles])

  uniqueXs <- unique(tiledim[, 1, drop = TRUE])
  uniqueYs <- unique(tiledim[, 2, drop = TRUE])
  if(length(uniqueXs) > 1) stop("More than one image x dimension: ",
                                paste(uniqueXs, collapse = ", "))
  if(length(uniqueYs) > 1) stop("More than one image y dimension: ",
                                paste(uniqueYs, collapse = ", "))

  # assign target dim with the max of z dimensions (so a band can be added)
  # if not all have the same bands
  targetdim <- c(uniqueXs, uniqueYs, max(tiledim[, 3, drop = TRUE]))

  # also return the number of missing tiles
  list(targetdim=targetdim, nmissing=sum(missing_tiles))
}

tile.plotfused <- function(tiles, zoom, type, epsg=4326, cachedir=NULL, quiet = FALSE) {
  fused <- tile.fuse(tiles, zoom, type, epsg=epsg, cachedir=cachedir, quiet = quiet)
  # plot image
  tile.plotarray(fused, attr(fused, "bbox"))
}

#' Plot Open Street Map Tiles
#'
#' Plot Open Street Map tiles using \code{rasterImage} and \code{sp::plot}.
#' Define your own tile sources by creating a tile url function in the
#' global environment, although most \href{http://wiki.openstreetmap.org/wiki/Tile_servers}{OSM listed}
#' servers are included. See \link{osm.types} for types options. By default tiles
#' are plotted in the Spherical Mercator projection
#' (\href{https://en.wikipedia.org/wiki/Web_Mercator}{epsg:3857}); pass \code{project=FALSE}
#' to keep lat/lon coordinates.
#'
#' @param bbox A bounding box as generated by \code{sp::bbox()} or \code{prettymapr::searchbbox()}
#' @param zoomin The amount by which to adjust the automatically calculated zoom (or
#' manually specified if the \code{zoom} parameter is passed). Use +1 to zoom in, or -1 to zoom out.
#' @param zoom Manually specify the zoom level (not reccomended; adjust \code{zoomin} or
#' \code{res} instead.
#' @param type A map type; one of that returned by \link{osm.types}. User defined types are possible
#' by defining \code{tile.url.TYPENAME <- function(xtile, ytile, zoom){}} and passing TYPENAME
#' as the \code{type} argument.
#' @param forcedownload \code{TRUE} if cached tiles should be re-downloaded. Useful if
#' some tiles are corrupted.
#' @param stoponlargerequest By default \code{osm.plot} will only load 32 tiles at a time. If
#' plotting at a higher resolution it may be necessary to pass \code{true} here.
#' @param fusetiles \code{TRUE} if tiles should be fused into a single image. This is the
#' default because white lines appear between tiles if it is set to \code{FALSE}. PDFs
#' appear not to have this problem, so when plotting large, high resolution PDFs it may be
#' faster (and more memory efficient) to use \code{fusetiles=FALSE}.
#' @param cachedir The directory in which tiles should be cached. Defaults to \code{getwd()/rosm.cache}.
#' @param res The resolution used to calculate scale.
#' @param project \code{TRUE} if tiles should be projected to a pseudo-mercator projection,
#' \code{FALSE} if lat/lon should be maintained. Becuase \code{sp::plot} adjusts the aspect
#' according to latitude for lat/lon coordinates, this makes little difference at high
#' zoom and may make plotting overlays more convenient. Defaults to \code{TRUE}.
#' @param progress A progress bar to use, or "none" to suppress progress updates
#' @param quiet Pass \code{FALSE} to see more error messages, particularly if
#'   your tiles do not download/load properly.
#' @param ... Additional parameters to be passed on to the first call to \code{sp::plot}
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(prettymapr)
#' ns <- makebbox(47.2, -59.7, 43.3, -66.4)
#' osm.plot(ns)
#' osm.plot(ns, type="stamenbw")
#' prettymap(osm.plot(ns), scale.style="ticks", scale.tick.cex=0)
#' }
osm.plot <- function(bbox, zoomin=0, zoom=NULL, type=NULL, forcedownload=FALSE,
                     stoponlargerequest=TRUE, fusetiles=TRUE, cachedir=NULL, res=150,
                     project=TRUE, progress=c("text", "none"), quiet = TRUE, ...) {
  # validate progress arg
  progress <- match.arg(progress)

  # get lookup information from input
  bbox <- extract_bbox(bbox)

  # verify tile source
  if(is.null(type)) {
    type <- get_default_tile_source()
  } else {
    type <- as.tile_source(type)
  }

  if(project) {
    epsg <- 3857
  } else {
    epsg <- 4326
  }

  bbox <- .projectbbox(bbox, epsg)

  coords <- sp::coordinates(t(bbox))
  spoints = sp::SpatialPoints(coords, proj4string = sp::CRS(paste0("+init=epsg:", epsg)))

  plotargs <- list(...)
  if(is.null(plotargs$xlim))
    xlim <- bbox[1,]
  if(is.null(plotargs$ylim))
    ylim <- bbox[2,]

  sp::plot(spoints, pch=".", xlim=xlim, ylim=ylim, ...)

  if(is.null(zoom)) {
    zoom <- tile.autozoom(res=res, epsg=epsg)
  }
  zoom <- zoom+zoomin
  maxzoom <- tile.maxzoom(type)
  zoom <- min(zoom, maxzoom)

  #global min zoom set to 1
  zoom <- max(1, zoom)
  message("Zoom: ", zoom)
  #adjust bbox to final plot extents
  bbox <- t(matrix(graphics::par('usr'), ncol=2, byrow=FALSE))

  tiles <- tiles.bybbox(bbox, zoom, epsg=epsg)
  if((nrow(tiles)>32) && stoponlargerequest) stop("More than 32 tiles to be loaded. ",
                                                  "Run with stoponlargerequest=FALSE or ",
                                                  "zoomin=-1, to continue")
  tile.download(tiles, zoom, type=type, forcedownload=forcedownload, cachedir=cachedir,
                progress=progress, quiet = quiet)

  if(fusetiles) {
    tile.plotfused(tiles, zoom, type=type, epsg=epsg, cachedir=cachedir, quiet = quiet)
  } else {
    tile.ploteach(tiles, zoom, type=type, epsg=epsg, cachedir=cachedir, quiet = quiet)
  }

  tile.attribute(type)
}
