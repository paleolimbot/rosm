# rosm 0.3.1

* Built-in CARTO map types now accept an `api_key` argument or use the
  `CARTO_API_KEY` environment variable, as required by CARTO's basemap service.

* Fix outdated documentation syntax (#110)
* Update GitHub Actions testing
* Skip tests for built-in URLs for Bing Maps and Stamen maps as the
  hard-coded URLs no longer load.

# rosm 0.3.0

* Silent deprecation the entire previous API (#20).
* Drop rgdal dependency (#21).
* Add new API based on wk and the curl package's multi
  download interface (#23).
* Update test and CI infrastructure (#27).
* Added NEWS.md to track changes in this package.

# rosm 0.2.5

* Updates to ensure compliance with CRAN policies.
