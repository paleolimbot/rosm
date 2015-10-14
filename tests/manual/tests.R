#tests
library(rcanvec)
library(prettymapr)
library(maptools)
library(sp)

nsbox <- prettymapr::searchbbox("nova scotia")
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


#this doesn't load properly
prettymap({
gmap.plot(prettymapr::searchbbox("2772 greenfield rd gaspereau NS", source="google"), project=FALSE)
rd <- canvec.load(nts("21h1"), "road")
plot(rd, add=T, lwd=4)
})

#this doesn't seem to load properly
prettymap({
gmap.plot(prettymapr::searchbbox("blomidon, NS", source="google"), project=F)
  plot(canvec.load(nts("21h1"), "waterbody"), add=T, lwd=2)
})

#test on small scale canadian locations to check alignment
smalllocs <- c("wolfville NS", "blomidon, NS",
          "calgary, ab", "whistler, BC", "fredericton, NB",
          "cedar lake, algonquin park, ON", "alta lake BC")

#osm
type <- "stamenwatercolor"

#plot the whole world
osm.plot(makebbox(89.9, 179.9, -89.9, -179.9))

for(loc in smalllocs) {
  cat(loc, "\n")
  box <- prettymapr::searchbbox(loc, source="google")
  cat(box, "\n")
  prettymap({osm.plot(box, type=type, project=F)
    rcanvec::canvec.qplot(bbox=box, layers="waterbody", add=T)
    title(paste(loc, type))})
}

#gmap
for(loc in smalllocs) {
  cat(loc, "\n")
  box <- prettymapr::searchbbox(loc, source="google")
  cat(box, "\n")
  prettymap({gmap.plot(box, project=F)
            rcanvec::canvec.qplot(bbox=box, layers="waterbody", add=T)
            title(loc)})
}

biglocs <- c("nova scotia", "united states", "canada", "alberta")
data("wrld_simpl")
canada <- wrld_simpl[wrld_simpl$NAME=="Canada",]
usa <- wrld_simpl[wrld_simpl$NAME=="United States",]
canada <- spTransform(canada, CRS("+init=epsg:3857"))
usa <- spTransform(usa, CRS("+init=epsg:3857"))

#osm
type <- "stamenwatercolor"
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

#gmap
for(loc in biglocs) {
  cat(loc, "\n")
  box <- prettymapr::searchbbox(loc, source="google")
  cat(box, "\n")
  prettymapr::prettymap({gmap.plot(box, project=T, asp=1)
    title(loc)})
}
