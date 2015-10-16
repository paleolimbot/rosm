#tile URLs

tile.url.osm <- function(xtile, ytile, zoom) {
  #a. b. or c. all work
  servers <- c("http://a.tile.openstreetmap.org",
               "http://b.tile.openstreetmap.org",
               "http://c.tile.openstreetmap.org")
  return(paste(paste(sample(servers, 1),
                     zoom, xtile, ytile, sep="/"),".png", sep=""))
}

tile.url.mapquestosm <- function(xtile, ytile, zoom) {
  servers <- c("http://otile1.mqcdn.com/tiles/1.0.0/osm",
               "http://otile2.mqcdn.com/tiles/1.0.0/osm",
               "http://otile3.mqcdn.com/tiles/1.0.0/osm",
               "http://otile4.mqcdn.com/tiles/1.0.0/osm")
  return(paste(paste(sample(servers, 1),
                     zoom, xtile, ytile, sep="/"),".jpg", sep=""))
}

tile.maxzoom.mapquestsat <- function() {return(8)}
tile.url.mapquestsat <- function(xtile, ytile, zoom) {
  servers <- c("http://otile1.mqcdn.com/tiles/1.0.0/sat",
               "http://otile2.mqcdn.com/tiles/1.0.0/sat",
               "http://otile3.mqcdn.com/tiles/1.0.0/sat",
               "http://otile4.mqcdn.com/tiles/1.0.0/sat")
  return(paste(paste(sample(servers, 1),
                     zoom, xtile, ytile, sep="/"),".jpg", sep=""))
}

tile.url.opencycle <- function(xtile, ytile, zoom) {
  servers <- c("http://a.tile.opencyclemap.org/cycle",
               "http://b.tile.opencyclemap.org/cycle")
  return(paste(paste(sample(servers, 1),
                     zoom, xtile, ytile, sep="/"),".png", sep=""))
}

tile.url.hotstyle <- function(xtile, ytile, zoom) {
  servers <- c("http://a.tile.openstreetmap.fr/hot",
               "http://a.tile.openstreetmap.fr/hot")
  return(paste(paste(sample(servers, 1),
                     zoom, xtile, ytile, sep="/"),".png", sep=""))
}

tile.url.openpiste <- function(xtile, ytile, zoom) {
  return(paste(paste("http://tiles.openpistemap.org/nocontours",
                     zoom, xtile, ytile, sep="/"),".png", sep=""))
}

tile.url.lovinahike <- function(xtile, ytile, zoom) {
  return(paste(paste("http://tile.lonvia.de/hiking",
                     zoom, xtile, ytile, sep="/"),".png", sep=""))
}

tile.url.lovinacycle <- function(xtile, ytile, zoom) {
  return(paste(paste("http://tile.waymarkedtrails.org/cycling",
                     zoom, xtile, ytile, sep="/"),".png", sep=""))
}

tile.url.hikebike <- function(xtile, ytile, zoom) {
  return(paste(paste("http://toolserver.org/tiles/hikebike",
                     zoom, xtile, ytile, sep="/"),".png", sep=""))
}

tile.maxzoom.hillshade <- function() {return(14)}
tile.url.hillshade <- function(xtile, ytile, zoom) {
  return(paste(paste("http://c.tiles.wmflabs.org/hillshading",
                     zoom, xtile, ytile, sep="/"),".png", sep=""))
}

tile.url.osmgrayscale <- function(xtile, ytile, zoom) {
  servers <- c("http://a.www.toolserver.org/tiles/bw-mapnik",
               "http://b.www.toolserver.org/tiles/bw-mapnik")
  return(paste(paste(sample(servers, 1),
                     zoom, xtile, ytile, sep="/"),".png", sep=""))
}

tile.url.stamenbw <- function(xtile, ytile, zoom) {
  return(paste(paste("http://a.tile.stamen.com/toner",
                     zoom, xtile, ytile, sep="/"),".png", sep=""))
}

tile.url.stamenwatercolor <- function(xtile, ytile, zoom) {
  return(paste(paste("http://a.tile.stamen.com/watercolor",
                     zoom, xtile, ytile, sep="/"),".jpg", sep=""))
}

tile.url.osmtransport <- function(xtile, ytile, zoom) {
  servers <- c("http://a.tile2.opencyclemap.org/transport",
               "http://b.tile2.opencyclemap.org/transport")
  return(paste(paste(sample(servers, 1),
                     zoom, xtile, ytile, sep="/"),".png", sep=""))
}


tile.url.thunderforestlandscape <- function(xtile, ytile, zoom) {
  servers <- c("http://a.tile.thunderforest.com/landscape",
               "http://b.tile.thunderforest.com/landscape",
               "http://c.tile.thunderforest.com/landscape")
  return(paste(paste(sample(servers, 1),
                     zoom, xtile, ytile, sep="/"),".png", sep=""))
}

tile.url.thunderforestoutdoors <- function(xtile, ytile, zoom) {
  servers <- c("http://a.tile.thunderforest.com/outdoors",
               "http://b.tile.thunderforest.com/outdoors",
               "http://c.tile.thunderforest.com/outdoors")
  return(paste(paste(sample(servers, 1),
                     zoom, xtile, ytile, sep="/"),".png", sep=""))
}

tile.url.bingaerial <- function(xtile, ytile, zoom) {
  #http://ecn.t2.tiles.virtualearth.net/tiles/a331.jpeg?g=587
  servers = c("t1", "t2", "t3", "t4")
  paste0("http://ecn.", sample(servers, 1), ".tiles.virtualearth.net/tiles/a",
         bmaps.quadkey(xtile, ytile, zoom), ".jpeg?g=", sample(0:1111, 1))
}
