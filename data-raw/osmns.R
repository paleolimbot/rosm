
library(rosm)

bounds <- wk::rct(
  252185, 4815826, 739729, 5210280,
  crs = "EPSG:32620"
)

for (zoom in 0:7) {
  tiles <- osm_tile_covering(bounds, zoom = zoom)
  osm_url_load_async(
    tiles,
    osm_url_spec(),
    cache_spec = osm_url_spec("inst/extdata/osmns/{z}_{x}_{y}.png"),
  )
}

list.files("inst/extdata/osmns")
