#tests

types <- c("hikebike","hillshade","hotstyle","lovinacycle",
           "lovinahike","mapquestosm","mapquestsat","opencycle",
           "openpiste","osm","osmgrayscale",
           "osmtransport","stamenbw","stamenwatercolor",
           "thunderforestlandscape","thunderforestoutdoors")

for(type in types) {
  osm.plot(nsbox, type=type)
  title(type)
}

#this doesn't seem to load properly
gmap.plot(prettymapr::searchbbox("blomidon, NS", source="google"), project=FALSE)
rcanvec::canvec.qplot(nts('21h1'), layers="waterbody", add=T)
