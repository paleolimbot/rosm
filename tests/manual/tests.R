
# tests
library(prettymapr)
library(rosm)

tiles <- data.frame(types = osm.types(), status = NA,
                    stringsAsFactors = FALSE)

nsbox <- searchbbox("nova scotia", source="google")
for(i in 1:nrow(tiles)) {
  status <- tryCatch(
    {
      osm.plot(nsbox, type = tiles[i,1])
      status <- "OK"
    },
    error=function(cond) {
      message(cond, '\n')
      status <- "error"
    }
  )
  tiles[i,"status"] <- status
}

knitr::kable(tiles)

# test custom map types
tile.url.darkmatter <- function(xtile, ytile, zoom) {
  paste0(paste("http://a.basemaps.cartocdn.com/dark_all",
               zoom, xtile, ytile, sep="/"), ".png")
}
osm.plot(nsbox, type="darkmatter")

# test bing map types
tiles <- data.frame(types = bmaps.types(), status = NA,
                    stringsAsFactors = FALSE)

nsbox <- searchbbox("nova scotia", source="google")
for(i in 1:nrow(tiles)) {
  status <- tryCatch(
    {
      bmaps.plot(nsbox, type = tiles[i,1])
      status <- "OK"
    },
    error=function(cond) {
      message(cond, '\n')
      status <- "error"
    }
  )
  tiles[i,"status"] <- status
}

# canvec.qplot and hillshade
prettymap({
  rcanvec::canvec.qplot(bbox=prettymapr::searchbbox("Alta Lake BC", source="google"),
               layers=c("waterbody", "forest", "river", "road"))
  osm.plot(bbox=prettymapr::searchbbox("Alta Lake BC", source="google"),
           type="hillshade", add=T, project = F)
})


# plot the whole world (still doesn't work)
# osm.plot(makebbox(89.9, 179.9, -89.9, -179.9), zoom=0)
# prettymap(osm.plot(makebbox(89.9, 179.9, -89.9, -179.9)))

# plot wrap around situations
osm.plot(zoombbox(makebbox(89.9, 179.9, -89.9, -179.9), 2, c(-100, 0)), zoomin=1)
osm.plot(zoombbox(makebbox(89.9, 179.9, -89.9, -179.9), 2, c(-100, 0)), zoomin=1, project=F)
osm.plot(searchbbox("alaska", source="google"))
osm.plot(searchbbox("alaska", source="google"), project=F)
bmaps.plot(searchbbox("alaska", source="google"))

# wrap around for projected version of Alaska does not work
# x <- osm.raster(searchbbox("alaska", source="google"),
#                 projection=CRS("+init=epsg:3857"))
# plotRGB(x)

# osm.raster
library(cartography)
data(nuts2006)
for(country in c("PL", "PT")) {
  message("Testing country ", country)
  spdf <- nuts0.spdf[nuts0.spdf$id==country,]
  x <- osm.raster(spdf, type="osm")
  plotRGB(x)
  plot(spdf, add=T)
}

# make more rasters (projected)
ns <- makebbox(47.2, -59.7, 43.3, -66.4)
x <- osm.raster(ns, projection=CRS("+init=epsg:26920"), crop=TRUE)
plotRGB(x)

# make non-projected raster
x <- osm.raster(ns)
plotRGB(x)

# make cropped raster
x <- osm.raster(ns, crop=TRUE)
plotRGB(x)

# write to disk check
osm.raster(x, filename="test.tif")
file.exists("test.tif")
x <- raster("test.tif")
osm.raster(ns, projection=CRS("+init=epsg:26920"), crop=T,
           filename="test.tif", overwrite=TRUE)
