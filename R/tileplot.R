#plot slippy tiles

tile.ploteach <- function(tiles, zoom, type, epsg=4326, cachedir=NULL) {
  for(i in 1:nrow(tiles)) {
    x <- tiles[i,1]
    y <- tiles[i,2]
    image <- png::readPNG(tile.cachename(x, y, zoom, type, cachedir))
    box <- tile.bbox(x, y, zoom, epsg)
    rasterImage(image, box[1,1], box[2,1], box[1,2], box[2,2])
  }
}


osm.plot <- function(bbox, zoomin=0, zoom=NULL, type="osm", forcedownload=FALSE,
                     stoponlargerequest=TRUE, cachedir=NULL, res=150, epsg=4326, ...) {
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

  #adjust bbox to final plot extents
  bbox <- t(matrix(par('usr'), ncol=2, byrow=FALSE))

  tiles <- tiles.bybbox(bbox, zoom, epsg=epsg)
  if((nrow(tiles)>32) && stoponlargerequest) stop("More than 32 tiles to be loaded. ",
                                                  "Run with stoponlargerequest=FALSE or ",
                                                  "zoomin=-1, to continue")
  tile.download(tiles, zoom, type=type, forcedownload=forcedownload, cachedir=cachedir)
  tile.ploteach(tiles, zoom, type=type, epsg=epsg, cachedir=cachedir)

}

types <- c(" hikebike " ," hillshade " ," hotstyle " ," lovinacycle " ,
           " lovinahike " ," mapquestosm " ," mapquestsat " ," opencycle " ,
           " openpiste " ," opensea " ," osm " ," osmgrayscale " ,
           " osmtransport " ," stamenbw " ," stamenwatercolor " ,
           " thunderforestlandscape " ," thunderforestoutdoors ")
