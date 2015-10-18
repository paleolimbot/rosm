\name{rosm-package}
\alias{rosm-package}
\alias{rosm}
\docType{package}
\title{
Plot Raster Map Tiles From Open Street Map and Other Sources
}
\description{
This package provides access and plots \href{http://www.openstreetmap.org/}{Open Street Map},
\href{http://www.mapquest.com/}{Mapquest}, and \href{http://www.bing.com/maps}{Bing Maps} tiles
to create high resolution basemaps and use hillshade tiles to add texture to other maps.
Uses the 'sp' package to plot using base graphics. Plot Open Street Map derivative
tiles using \link{osm.plot}, and plot Bing maps (Aerial, Labeled Aerial, Road) using
\link{bmaps.plot}. 16 OSM and 3 Bing sources are included, with the ability to define
custom tile sources based on OSM tilex, tiley, and zoom.
}
\author{
Dewey Dunnington <dewey@fishandwhistle.net>
}
\references{
   \href{http://wiki.openstreetmap.org/wiki/Tile_servers}{Open Street Map tile servers},
   \href{http://www.microsoft.com/maps/choose-your-bing-maps-API.aspx}{Bing Maps API documentation}
}
\keyword{ package }
\examples{

\donttest{
library(rcanvec)
library(prettymapr)
library(sp)

#basic ploting
nsbox <- searchbbox("nova scotia", source="google")
osm.plot(nsbox)
osm.plot(nsbox, type="mapquestsat")
bmaps.plot(nsbox)
bmaps.plot(nsbox, type="Road")

#use {prettymapr} to add scalebar and north arrow
prettymap(osm.plot(nsbox))
prettymap(bmaps.plot(nsbox, type="Road"))

#increase res argument to plot to file
pdf(height=8, width=10.5)
prettymap(osm.plot(nsbox, type="mapquestsat", res=300, stoponlargerequest=FALSE),
          scale.label.col="white", arrow.text.col = "white",
          scale.linecol = "white", arrow.border = "white")
dev.off()

#canvec.qplot and hillshade using the add=TRUE argument
prettymap({
  altabox <- prettymapr::searchbbox("Alta Lake BC", source="google")
  canvec.qplot(bbox=altabox,
               layers=c("waterbody", "forest", "river", "road"))
  osm.plot(altabox, type="hillshade", add=T, project = FALSE)
})

#define custom types by defining a tile.url.TYPE function
tile.url.darkmatter <- function(xtile, ytile, zoom) {
   paste0(paste("http://a.basemaps.cartocdn.com/dark_all",
                             zoom, xtile, ytile, sep="/"), ".png")
}
tile.maxzoom.darkmatter <- function() {return(19)} #useful it maxzoom is important
osm.plot(nsbox, type="darkmatter")

}

}
