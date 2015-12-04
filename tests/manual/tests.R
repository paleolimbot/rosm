#tests
library(rcanvec)
library(prettymapr)
library(maptools)
library(sp)
library(cartography)
library(raster)


nsbox <- prettymapr::searchbbox("nova scotia", source="google")
types <- c("hikebike","hillshade","hotstyle","lovinacycle",
           "lovinahike","mapquestosm","mapquestsat","opencycle",
           "openpiste","osm","osmgrayscale",
           "osmtransport","stamenbw","stamenwatercolor",
           "thunderforestlandscape","thunderforestoutdoors")

for(type in types) {
  osm.plot(nsbox, type=type)
  title(type)
}

#canvec.qplot and hillshade
prettymap({
  canvec.qplot(bbox=prettymapr::searchbbox("Alta Lake BC", source="google"),
               layers=c("waterbody", "forest", "river", "road"))
  osm.plot(bbox=prettymapr::searchbbox("Alta Lake BC", source="google"),
           type="hillshade", add=T, project = F)
})

# ##GMAPS ABANDONED THIS VERSION
# #this doesn't load properly
# prettymap({
# gmap.plot(prettymapr::searchbbox("2772 greenfield rd gaspereau NS", source="google"), project=T)
# rd <- canvec.load(nts("21h1"), "road")
# plot(spTransform(rd, CRS("+init=epsg:3857")), add=T, lwd=4)
# })
#
# #this doesn't seem to load properly
# prettymap({
# gmap.plot(prettymapr::searchbbox("blomidon, NS", source="google"), project=F)
#   plot(canvec.load(nts("21h1"), "waterbody"), add=T, lwd=2)
# })

#test on small scale canadian locations to check alignment
smalllocs <- c("wolfville NS", "blomidon, NS",
          "calgary, ab", "whistler, BC", "fredericton, NB",
          "cedar lake, algonquin park, ON", "alta lake BC")

#osm
type <- "thunderforestoutdoors"

for(loc in smalllocs) {
  cat(loc, "\n")
  box <- prettymapr::searchbbox(loc, source="google")
  cat(box, "\n")
  prettymap({osm.plot(box, type=type, project=F)
             title(paste(loc, type))})
}

# #gmap
# for(loc in smalllocs) {
#   cat(loc, "\n")
#   box <- prettymapr::searchbbox(loc, source="google")
#   cat(box, "\n")
#   prettymap({gmap.plot(box, project=F)
#             rcanvec::canvec.qplot(bbox=box, layers="waterbody", add=T)
#             title(loc)})
# }

#bmaps
bingtype <- "AerialWithLabels"
for(loc in smalllocs) {
  cat(loc, "\n")
  box <- prettymapr::searchbbox(loc, source="google")
  cat(box, "\n")
  prettymapr::prettymap({bmaps.plot(box, bingtype)
    title(loc)})
}

#plot the whole world (still doesn't work)
# osm.plot(makebbox(89.9, 179.9, -89.9, -179.9), zoom=0)
# prettymap(osm.plot(makebbox(89.9, 179.9, -89.9, -179.9)))

#plot wrap around situations
osm.plot(zoombbox(makebbox(89.9, 179.9, -89.9, -179.9), 2, c(-92, 0)), zoomin=1)
osm.plot(zoombbox(makebbox(89.9, 179.9, -89.9, -179.9), 2, c(-100, 0)), zoomin=1, project=F)
osm.plot(prettymapr::searchbbox("alaska", source="google"))
bmaps.plot(prettymapr::searchbbox("alaska", source="google"))

biglocs <- c("nova scotia", "united states", "canada", "alberta")
data("wrld_simpl")
canada <- wrld_simpl[wrld_simpl$NAME=="Canada",]
usa <- wrld_simpl[wrld_simpl$NAME=="United States",]
canada <- spTransform(canada, CRS("+init=epsg:3857"))
usa <- spTransform(usa, CRS("+init=epsg:3857"))

#osm
type <- "osm"
for(loc in biglocs) {
  cat(loc, "\n")
  box <- prettymapr::searchbbox(loc, source="google")
  cat(box, "\n")
  prettymap(
    {osm.plot(box, type=type)
     plot(canada, add=T)
     plot(usa, add=T)
     title(paste(loc, type))})
}

# #gmap
# for(loc in biglocs) {
#   cat(loc, "\n")
#   box <- prettymapr::searchbbox(loc, source="google")
#   cat(box, "\n")
#   prettymapr::prettymap({gmap.plot(box, project=T, asp=1)
#     title(loc)})
# }

#bingmaps
bingtype <- "Road"
for(loc in biglocs) {
  cat(loc, "\n")
  box <- prettymapr::searchbbox(loc, source="google")
  cat(box, "\n")
  prettymapr::prettymap({bmaps.plot(box, bingtype)
    title(loc)})
}

tile.url.darkmatter <- function(xtile, ytile, zoom) {
  paste0(paste("http://a.basemaps.cartocdn.com/dark_all",
               zoom, xtile, ytile, sep="/"), ".png")
}
osm.plot(nsbox, type="darkmatter")


#osm.raster
data(nuts2006)
for(country in unique(nuts0.spdf$id)) {
  message("Testing country ", country)
  spdf <- nuts0.spdf[nuts0.spdf$id==country,]
  spdf2 <- spTransform(spdf, CRSobj = CRS("+init=epsg:4326"))
  x <- osm.raster(bbox(spdf2), type="thunderforestlandscape")
  x2 <- osm.proj(x, spdf@proj4string, crop.bbox=bbox(spdf))
  osm.proj.plot(x2)
  title(country)
}



