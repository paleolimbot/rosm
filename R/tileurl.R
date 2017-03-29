#tile URLs

tile.url.osm <- function(xtile, ytile, zoom) {
  #a. b. or c. all work
  servers <- c("http://a.tile.openstreetmap.org",
               "http://b.tile.openstreetmap.org",
               "http://c.tile.openstreetmap.org")
  return(paste(paste(sample(servers, 1),
                     zoom, xtile, ytile, sep="/"),".png", sep=""))
}
tile.attribute.osm <- function() {
  message("Consider donating to the Open Street Map project at ",
          "http://donate.openstreetmap.org/")
}

tile.url.opencycle <- function(xtile, ytile, zoom) {
  servers <- c("http://a.tile.opencyclemap.org/cycle",
               "http://b.tile.opencyclemap.org/cycle")
  return(paste(paste(sample(servers, 1),
                     zoom, xtile, ytile, sep="/"),".png", sep=""))
}
tile.attribute.opencycle <- function() {
  message("Consider donating to the Open Street Map project at ",
          "http://donate.openstreetmap.org/")
}

tile.url.hotstyle <- function(xtile, ytile, zoom) {
  servers <- c("http://a.tile.openstreetmap.fr/hot",
               "http://a.tile.openstreetmap.fr/hot")
  return(paste(paste(sample(servers, 1),
                     zoom, xtile, ytile, sep="/"),".png", sep=""))
}
tile.attribute.hotstyle <- function() {
  message("Consider donating to the Open Street Map project at ",
          "http://donate.openstreetmap.org/")
}

tile.url.lovinahike <- function(xtile, ytile, zoom) {
  return(paste(paste("http://tile.waymarkedtrails.org/hiking",
                     zoom, xtile, ytile, sep="/"),".png", sep=""))
}
tile.attribute.loviniahike <- function() {
  message("Visit the Lovinia hike map at http://osm.lonvia.de/hiking.html")
}

tile.url.lovinacycle <- function(xtile, ytile, zoom) {
  return(paste(paste("http://tile.waymarkedtrails.org/cycling",
                     zoom, xtile, ytile, sep="/"),".png", sep=""))
}
tile.attribute.loviniacycle <- function() {
  message("Visit the Lovinia cycle map at http://cycling.waymarkedtrails.org/")
}

tile.url.hikebike <- function(xtile, ytile, zoom) {
  servers <- paste0("http://", c("a", "b", "c"), ".tiles.wmflabs.org/hikebike")
  return(paste(paste(sample(servers, 1),
                     zoom, xtile, ytile, sep="/"),".png", sep=""))
}
tile.attribute.hikebike <- function() {
  message("Visit the online map at http://hikebikemap.de/")
}

tile.maxzoom.hillshade <- function() {return(14)}
tile.url.hillshade <- function(xtile, ytile, zoom) {
  return(paste(paste("http://c.tiles.wmflabs.org/hillshading",
                     zoom, xtile, ytile, sep="/"),".png", sep=""))
}
tile.attribute.hillshade <- function() {
  message("This project appears to be part of the ",
          "WikiTech site https://wikitech.wikimedia.org/wiki/Main_Page")
}

tile.url.osmgrayscale <- function(xtile, ytile, zoom) {
  # a b and c cause SSL certificate errors
  servers <- c("https://tiles.wmflabs.org/bw-mapnik")
  return(paste(paste(sample(servers, 1),
                     zoom, xtile, ytile, sep="/"),".png", sep=""))
}
tile.attribute.osmgrayscale <- function() {
  message("Consider donating to the Open Street Map project at ",
          "http://donate.openstreetmap.org/")
}

tile.url.stamenbw <- function(xtile, ytile, zoom) {
  return(paste(paste("http://a.tile.stamen.com/toner",
                     zoom, xtile, ytile, sep="/"),".png", sep=""))
}
tile.attribute.stamenbw <- function() {
  message("Visit the Stamen cartography website at http://maps.stamen.com/")
}


tile.url.stamenwatercolor <- function(xtile, ytile, zoom) {
  return(paste(paste("http://a.tile.stamen.com/watercolor",
                     zoom, xtile, ytile, sep="/"),".jpg", sep=""))
}
tile.attribute.stamenwatercolor <- function() {
  message("Visit the Stamen cartography website at http://maps.stamen.com/")
}

tile.url.osmtransport <- function(xtile, ytile, zoom) {
  servers <- c("http://a.tile2.opencyclemap.org/transport",
               "http://b.tile2.opencyclemap.org/transport")
  return(paste(paste(sample(servers, 1),
                     zoom, xtile, ytile, sep="/"),".png", sep=""))
}
tile.attribute.osmtransport <- function() {
  message("Consider donating to the Open Street Map project at ",
          "http://donate.openstreetmap.org/")
}


tile.url.thunderforestlandscape <- function(xtile, ytile, zoom) {
  servers <- c("http://a.tile.thunderforest.com/landscape",
               "http://b.tile.thunderforest.com/landscape",
               "http://c.tile.thunderforest.com/landscape")
  return(paste(paste(sample(servers, 1),
                     zoom, xtile, ytile, sep="/"),".png", sep=""))
}
tile.attribute.thunderforestlandscape <- function() {
  message("More on Thunderforest at http://www.thunderforest.com/")
}

tile.url.thunderforestoutdoors <- function(xtile, ytile, zoom) {
  servers <- c("http://a.tile.thunderforest.com/outdoors",
               "http://b.tile.thunderforest.com/outdoors",
               "http://c.tile.thunderforest.com/outdoors")
  return(paste(paste(sample(servers, 1),
                     zoom, xtile, ytile, sep="/"),".png", sep=""))
}
tile.attribute.thunderforestoutdoors <- function() {
  message("More on Thunderforest at http://www.thunderforest.com/")
}

