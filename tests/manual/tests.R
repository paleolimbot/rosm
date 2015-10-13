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
rcanvec::canvec.qplot(rcanvec::nts('21h1'), layers="waterbody", add=T)

#test an arbitrary list of locations
locs <- c("wolfville NS", "nova scotia", "united states", "canada", "alberta",
          "calgary, ab", "vancouver island", "whistler, BC")

for(loc in locs) {
  cat(loc, "\n")
  box <- prettymapr::searchbbox(loc, source="google")
  cat(box, "\n")
  prettymapr::prettymap({gmap.plot(box, project=T, asp=1)
                        title(loc)})
}


