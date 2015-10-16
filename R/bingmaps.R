

bmaps.quadkey <- function(tilex, tiley, zoom) {
  nzoom <- 2^zoom
  if(tilex < 0 || tilex >= nzoom) stop("xtile out of range: ", tilex)
  if(tiley < 0 || tiley >= nzoom) stop("ytile out of range: ", tilex)
  out <- ""
  keymap <- matrix(0:3, byrow=TRUE, ncol=2)
  decx <- tilex/nzoom
  decy <- tiley/nzoom
  for(i in 1:zoom) {
    n <- 2^i
    x <- floor(decx*2^i) - floor(decx*2^(i-1))*2
    y <- floor(decy*2^i) - floor(decy*2^(i-1))*2
    out <- paste0(out, keymap[y+1,x+1])
  }
  as.integer(out)
}

bmaps.types <- function() {
  c("Aerial", "AerialWithLabels", "Road")
}

bmaps.restquery <- function(bingtype, key=NULL) {
  #http://dev.virtualearth.net/REST/v1/Imagery/Metadata/Aerial?key=KEY
  #get a key at https://msdn.microsoft.com/en-us/library/ff428642.aspx
  if(is.null(key)) {
    key <- "Aut49nhp5_Twwf_5RHF6wSGk7sEzpcSA__niIXCHowQZLMeC-m8cdy7EmZd2r7Gs"
  }
  urlstring <- paste0("http://dev.virtualearth.net/REST/v1/Imagery/Metadata/", bingtype, "?key=", key)
  message("Querying Bing REST API at ", urlstring)
  connect <- url(urlstring)
  lines <- try(readLines(connect, warn = FALSE), silent = TRUE)
  close(connect)

  if(class(lines) == "try-error") stop("  Bing REST query failed for type: ", bingtype)

  result <- rjson::fromJSON(paste(lines, collapse = ""))
  result$resourceSets[[1]]$resources[[1]]
}

bmaps.tileurlfromrest <- function(restresult, tilex, tiley, zoom) {
  gsub("{subdomain}", sample(c("t1", "t2", "t3", "t4"), 1),
       gsub("{quadkey}", bmaps.quadkey(tilex, tiley, zoom), restresult$imageUrl, fixed=TRUE), fixed=TRUE)
}

bmaps.plot <- function(bbox, bingtype="Aerial", key=NULL, ...) {
  if(!(bingtype %in% bmaps.types())) stop("bingtype must be one of Aerial, AerialWithLabels, or Road")
  rest <- bmaps.restquery(bingtype)
  tile.minzoom.tmp <<- function() {rest$zoomMin}
  tile.maxzoom.tmp <<- function() {rest$zoomMax}
  tile.url.tmp <<- function(xtile, ytile, zoom) {
    bmaps.tileurlfromrest(rest, xtile, ytile, zoom)
  }
  osm.plot(bbox=bbox, type="tmp", ...)
  rm(tile.minzoom.tmp, inherits=TRUE)
  rm(tile.maxzoom.tmp, inherits=TRUE)
  rm(tile.url.tmp, inherits=TRUE)
}

#http://ecn.t2.tiles.virtualearth.net/tiles/a331.jpeg?g=587
#/tiles/a" + quadtree + "." + getExtension() + "?g=587";


