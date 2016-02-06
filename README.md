# ROSM: Open Street Map tiles in R

[![](http://cranlogs.r-pkg.org/badges/rosm)](http://cran.rstudio.com/web/packages/rcanvec/index.html)

Download and plot Open Street Map <http://www.openstreetmap.org/>, Mapquest <http://www.mapquest.com/>, Bing Maps <http://www.bing.com/maps> and other tiled map sources in a way that works seamlessly with plotting from the 'sp' package. Use to create high-resolution basemaps and add hillshade to vector based maps.

The gist of it:

```R
install.packages("rosm") 
install.packages("prettymapr") #if these are not installed already

library(rosm)
library(prettymapr)
osm.plot(searchbbox("wolfville, ns"))
bmaps.plot(searchbbox("wolfville, ns"))

#or use prettymapr to remove margins and add scale bar
prettymap(bmaps.plot(searchbbox("wolfville, ns")))
```

Find more information on the [CRAN package page](https://cran.r-project.org/package=rosm) or [view the manual](https://cran.r-project.org/web/packages/rosm/rosm.pdf).
