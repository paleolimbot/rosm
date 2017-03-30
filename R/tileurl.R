# tile URLs
# create list() of tile sources
tile_sources <- list(
  osm = source_from_url_format(
    url_format = c('http://a.tile.openstreetmap.org/${z}/${x}/${y}.png',
                   'http://b.tile.openstreetmap.org/${z}/${x}/${y}.png',
                   'http://c.tile.openstreetmap.org/${z}/${x}/${y}.png'),
    attribution = paste("Consider donating to the Open Street Map project",
                        "at http://donate.openstreetmap.org/")
  ),

  opencycle = source_from_url_format(
    url_format = c('http://a.tile.opencyclemap.org/cycle/${z}/${x}/${y}.png',
                   'http://b.tile.opencyclemap.org/cycle/${z}/${x}/${y}.png'),
    attribution = paste("Consider donating to the Open Street Map project",
                        "at http://donate.openstreetmap.org/")
  ),

  hotstyle = source_from_url_format(
    url_format = c('http://a.tile.openstreetmap.fr/hot/${z}/${x}/${y}.png',
                   'http://b.tile.openstreetmap.fr/hot/${z}/${x}/${y}.png'),
    attribution = paste("Consider donating to the Open Street Map project",
                        "at http://donate.openstreetmap.org/")
  ),

  loviniahike = source_from_url_format(
    url_format = 'http://tile.waymarkedtrails.org/hiking/${z}/${x}/${y}.png',
    attribution = "Visit the Lovinia hike map at http://osm.lonvia.de/hiking.html"
  ),

  loviniacycle = source_from_url_format(
    url_format = 'http://tile.waymarkedtrails.org/cycling/${z}/${x}/${y}.png',
    attribution = "Visit the Lovinia cycle map at http://cycling.waymarkedtrails.org/"
  ),

  hikebike = source_from_url_format(
    url_format = paste0("http://", c("a", "b", "c"), ".tiles.wmflabs.org/hikebike/${z}/${x}/${y}.png"),
    attribution = "Visit the online map at http://hikebikemap.de/"
  ),

  hillshade = source_from_url_format(
    url_format = 'http://c.tiles.wmflabs.org/hillshading/${z}/${x}/${y}.png',
    attribution = paste("This project appears to be part of the",
                        "WikiTech site https://wikitech.wikimedia.org/wiki/Main_Page"),
    max_zoom = 14
  ),

  osmgrayscale = source_from_url_format(
    url_format = 'https://tiles.wmflabs.org/bw-mapnik/${z}/${x}/${y}.png',
    attribution = paste("Consider donating to the Open Street Map project",
                        "at http://donate.openstreetmap.org/")
  ),

  stamenbw = source_from_url_format(
    url_format = 'http://a.tile.stamen.com/toner/${z}/${x}/${y}.png',
    attribution = "Visit the Stamen cartography website at http://maps.stamen.com/"
  ),

  stamenwatercolor = source_from_url_format(
    url_format = 'http://a.tile.stamen.com/watercolor/${z}/${x}/${y}.jpg',
    attribution = "Visit the Stamen cartography website at http://maps.stamen.com/"
  ),

  osmtransport = source_from_url_format(
    url_format = c('http://a.tile2.opencyclemap.org/transport/${z}/${x}/${y}.png',
                   'http://b.tile2.opencyclemap.org/transport/${z}/${x}/${y}.png'),
    attribution = paste("Consider donating to the Open Street Map project",
                        "at http://donate.openstreetmap.org/")
  ),

  thunderforestlandscape = source_from_url_format(
    url_format = c('http://a.tile.thunderforest.com/landscape/${z}/${x}/${y}.png',
                   'http://b.tile.thunderforest.com/landscape/${z}/${x}/${y}.png',
                   'http://c.tile.thunderforest.com/landscape/${z}/${x}/${y}.png'),
    attribution = "More on Thunderforest at http://www.thunderforest.com/"
  ),

  thunderforestoutdoors = source_from_url_format(
    url_format = c('http://a.tile.thunderforest.com/outdoors/${z}/${x}/${y}.png',
                   'http://b.tile.thunderforest.com/outdoors/${z}/${x}/${y}.png',
                   'http://c.tile.thunderforest.com/outdoors/${z}/${x}/${y}.png'),
    attribution = "More on Thunderforest at http://www.thunderforest.com/"
  ),

  cartodark = source_from_url_format(
    url_format = "http://a.basemaps.cartocdn.com/light_all/${z}/${x}/${y}.png",
    attribution = "Map tiles by Carto, under CC BY 3.0. Data by OpenStreetMap, under ODbL."
  ),

  cartolight = source_from_url_format(
    url_format = "http://a.basemaps.cartocdn.com/dark_all/${z}/${x}/${y}.png",
    attribution = "Map tiles by Carto, under CC BY 3.0. Data by OpenStreetMap, under ODbL."
  )
)
