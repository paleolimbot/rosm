

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
  out
}

bmaps.restquery <- function(bingtype, key=NULL) {
  #http://dev.virtualearth.net/REST/v1/Imagery/Metadata/Aerial?key=KEY
  #get a key at https://msdn.microsoft.com/en-us/library/ff428642.aspx
  if(is.null(key)) {
    key <- "Aut49nhp5_Twwf_5RHF6wSGk7sEzpcSA__niIXCHowQZLMeC-m8cdy7EmZd2r7Gs"
  }
  urlstring <- paste0("http://dev.virtualearth.net/REST/v1/Imagery/Metadata/", bingtype, "?key=", key)
  message("Querying Bing REST API")
  connect <- url(urlstring)
  lines <- try(readLines(connect, warn = FALSE), silent = TRUE)
  close(connect)

  if(class(lines) == "try-error") stop("  Bing REST query failed for type: ", bingtype)

  result <- rjson::fromJSON(paste(lines, collapse = ""))
  result$resourceSets[[1]]$resources[[1]]
}

bmaps.tileurlfromrest <- function(imageUrl, tilex, tiley, zoom) {
  gsub("{subdomain}", sample(c("t1", "t2", "t3", "t4"), 1),
       gsub("{quadkey}", bmaps.quadkey(tilex, tiley, zoom), imageUrl, fixed=TRUE), fixed=TRUE)
}

tile.url.bing <- function(typecode, ext, xtile, ytile, zoom) {
  #http://ecn.t2.tiles.virtualearth.net/tiles/a331.jpeg?g=587
  if(!exists(".bingtoken")) stop("use bmaps.plot() to plot Bing maps")
  if(is.null(.bingtoken)) stop("use bmaps.plot() to plot Bing maps")
  servers = c("t1", "t2", "t3", "t4")
  paste0("http://ecn.", sample(servers, 1), ".tiles.virtualearth.net/tiles/",
         typecode, bmaps.quadkey(xtile, ytile, zoom), ext, "?g=", .bingtoken)
}

tile.url.bing_Aerial <- function(xtile, ytile, zoom) {
  tile.url.bing("a", ".jpeg", xtile, ytile, zoom)
}

tile.url.bing_AerialWithLabels <- function(xtile, ytile, zoom) {
  tile.url.bing("h", ".jpeg", xtile, ytile, zoom)
}

tile.url.bing_Road <- function(xtile, ytile, zoom) {
  tile.url.bing("r", ".png", xtile, ytile, zoom)
}

#' List types of Bing Maps
#'
#' @return A list of valid bing map types
#' @export
#'
#' @examples
#' bmaps.types()
#'
bmaps.types <- function() {
  c("Aerial", "AerialWithLabels", "Road")
}

#' Plot Bing Maps
#'
#' @param bbox
#' @param bingtype
#' @param key
#' @param ...
#'
#' @export
#'
bmaps.plot <- function(bbox, bingtype="Aerial", key=NULL, ...) {
  if(!(bingtype %in% bmaps.types())) stop("bingtype must be one of Aerial, AerialWithLabels, or Road")
  type <- paste("bing", bingtype, sep="_")
  rest <- bmaps.restquery(bingtype, key)
  afterg <- strsplit(rest$imageUrl, "?g=", fixed=TRUE)[[1]][2]
  .bingtoken <<- strsplit(afterg, "&", fixed=TRUE)[[1]][1]
  osm.plot(bbox=bbox, type=type, ...)
  .bingtoken <<- NULL
}

