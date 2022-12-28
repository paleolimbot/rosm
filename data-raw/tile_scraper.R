
# script to find tile formats

curl::curl_download("https://wiki.openstreetmap.org/wiki/Tile_servers", "data-raw/tileservers.html")

xml <- XML::htmlParse("tileservers.html")
links <- XML::getHTMLLinks("data-raw/tileservers.html")
links <- links[grepl("\\$\\{.\\}", links)]

# group a/b/c tile
linkcat <- gsub("http://[abc]\\.(.*)", "http://\\1", links)
linklist <- tapply(links, linkcat, function(v) paste0("'", v, "'", collapse = ", "),
  simplify = FALSE
)
names(linklist) <- NULL
print(linklist)
