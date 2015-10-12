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
