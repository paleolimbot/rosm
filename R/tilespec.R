

#' Tile Sources
#'
#' Tile sources define where rosm looks for map tiles. There are a number of
#' built-in types (\link{osm.types}), or they can be created using \code{as.tile_source()},
#' registered using \link{register_tile_source} for easy access, or passed directly to
#' the \link{osm.plot} family of methods.
#'
#' Passing a name from \link{osm.types} will return that tile source; passing a name from
#' \link{register_tile_source} will return that tile source, and passing a URL format in the form
#' \code{https://tiles.wmflabs.org/bw-mapnik/${z}/${x}/${y}.png} will create a new tile source.
#' Old style function names in the form tile.url.TYPE are still supported but are deprecated.
#'
#' @param x An object (usually a name or string format) with which to create a tile source
#' @param ... Arguments passed to other methods
#'
#' @return An object of class 'tile_source'
#' @export
#'
#'
#' @examples
#' as.tile_source("osm")
#'
as.tile_source <- function(x, ...) {
  # first check if x is a tile source
  if(is.tile_source(x)) return(x)

  # order is registered -> built_in -> functions -> string format
  if((length(x) == 1) && (x %in% names(registered_sources))) {
    src <- registered_sources[[x]]
    src$name <- x
    src
  } else if((length(x) == 1) && (x %in% names(tile_sources))) {
    src <- tile_sources[[x]]
    src$name <- x
    src
  } else if( (length(x) == 1) && is.character(x)) {
    # if url function exists, use old-style function definitions
    if(exists(paste0("tile.url.", x))) {
      source_from_global_functions(x)
    } else {
      # create using string format
      source_from_url_format(x, ...)
    }
  } else if((length(x) > 1) && is.character(x)) {
    # character vectors > 1 can't be functions
    source_from_url_format(x, ...)
  } else {
    stop("Don't know how to create a tile_source from type ", class(x))
  }
}

#' @rdname as.tile_source
#' @export
is.tile_source <- function(x) {
  inherits(x, "tile_source")
}

#' @rdname as.tile_source
#' @export
source_from_url_format <- function(url_format, max_zoom = tile.maxzoom.default(),
                                   min_zoom = 0, attribution = NULL, ...) {
  # url format like this: https://tiles.wmflabs.org/bw-mapnik/${z}/${x}/${y}.png
  # ${q} for quadkey

  # check format: need ${z}/${x}/${y} xor ${q} (not both)
  # url_format may be a vector of names, so wrap in all()
  if(xor(!all(grepl("${q}", url_format, fixed = TRUE)),
         !(!all(grepl("${z}", url_format, fixed = TRUE)) &&
           !all(grepl("${x}", url_format, fixed = TRUE)) &&
           !all(grepl("${y}", url_format, fixed = TRUE))))) {
    stop("url_format must contain ${q} xor ${z} and ${x} and ${y}")
  }

  # force args, since they will be used in closures
  force(max_zoom)
  force(attribution)
  extra_args <- list(...)

  create_tile_source(
    get_tile_url = function(xtile, ytile, zoom, quadkey = "") {
      withx <- gsub("${x}", xtile, sample(url_format, 1), fixed = TRUE)
      withy <- gsub("${y}", ytile, withx, fixed = TRUE)
      withz <- gsub("${z}", zoom, withy, fixed = TRUE)
      # return with quadkey
      gsub("${q}", quadkey, withz, fixed = TRUE)
    },
    get_attribution = function() attribution,
    get_max_zoom = function() max_zoom,
    get_min_zoom = function() min_zoom,
    name = url_format[1],
    url_formats = url_format,
    ...
  )
}

# these are old-style custom tile naming functions
source_from_global_functions <- function(type) {
  message("Using functions as custom tile sources is deprecated. Use string formats instead.")

  # get_url is mandatory, so use match.fun
  get_url <- match.fun(paste0("tile.url.", type))

  # maxzoom function is not manditory
  if(exists(paste0("tile.maxzoom.", type))) {
    get_max_zoom <- match.fun(paste0("tile.maxzoom.", type))
  } else {
    get_max_zoom <- tile.maxzoom.default
  }

  # attribute function is not manditory
  if(exists(paste0("tile.attribute.", type))) {
    attribution <- match.fun(paste0("tile.attribute", type))
  } else {
    attribution <- function() NULL
  }

  # return tile source
  create_tile_source(
    get_tile_url = get_url,
    get_max_zoom = get_max_zoom,
    get_min_zoom = tile.minzoom.default,
    get_attribution = attribution,
    name = type
  )
}


#' Register Tile Sources
#'
#' Use this function to register tile sources so they can be referred to by name in
#' \link{osm.plot}. Tile sources will be registered for as long as the namespace
#' is loaded. Use
#'
#' @param x The tile source (or coercible string) to use as the default tile source
#' @param ... Passed to \link{as.tile_source} for set_default_tile_source, or a named
#'   list of tile sources for register_tile_source
#'
#' @return
#' @export
#'
#' @examples
register_tile_source <- function(...) {
  sources <- list(...)
  if(is.null(names(sources))) stop("register_source must be called with named arguments")
  if(any(nchar(names(sources)) == 0)) stop("register_source must be called with named arguments")

  # lapply as.tile_source and copy to registered_sources
  list2env(lapply(sources, as.tile_source), registered_sources)

  invisible(NULL)
}

#' @rdname register_tile_source
#' @export
set_default_tile_source <- function(x, ...) {
  ts <- as.tile_source(x, ...)
  registered_sources$.defaultsource <- ts
}

#' @rdname register_tile_source
#' @export
get_default_tile_source <- function() {
  if(exists(".defaultsource", where = registered_sources)) {
    registered_sources$.defaultsource
  } else {
    as.tile_source("osm")
  }
}

# use an environment to keep track of registered sources
registered_sources <- new.env(parent = emptyenv())

#' Get List of Valid Tile Sources
#'
#' @return A character vector of valid \code{type} parameters.
#'
#' @export
#'
#' @examples
#' osm.types()
#'
osm.types <- function() {
  c(names(tile_sources), setdiff(names(registered_sources), ".defaultsource"))
}

# base function to create specs
create_tile_source <- function(get_tile_url, get_max_zoom, get_min_zoom, get_attribution, ...) {
  # here, get_tile_url, get_max_zoom, and get_attribution are all functions
  # that get passed xtile, ytile, zoom, and quadkey (if quadkey is in the formals)
  # this is for backwards compatiblity with older tile.url.TYPE functions

  # validate the functions. this also has the effect of evaluating them.

  # check that get_tile_url returns a character vector of length 1
  if("quadkey" %in% names(formals(get_tile_url))) {
    url <- get_tile_url(0, 0, 0, quadkey="0")
  } else {
    url <- get_tile_url(0, 0, 0)
  }
  if(!is.character(url)) stop("get_tile_url must return type 'character'")
  if(length(url) != 1) stop("get_tile_url must return a vector of length 1")

  # check that maxzoom is an integer
  maxzoom <- get_max_zoom()
  if((maxzoom %% 1) != 0) stop("get_max_zoom must return an integer")

  # check that minzoom is an integer
  minzoom <- get_min_zoom()
  if((minzoom %% 1) != 0) stop("get_min_zoom must return an integer")

  # check that get_attribution returns a character vector of length 1
  attribution <- get_attribution()
  if(!is.null(attribution)) {
    if(!is.character(attribution)) stop("get_attribution must return a character vector")
    if(length(url) != 1) stop("get_attribution must return a vector of length 1")
  }

  # return list of class "tile_source"
  structure(list(
    get_tile_url = get_tile_url,
    get_attribution = get_attribution,
    get_max_zoom = get_max_zoom,
    get_min_zoom = get_min_zoom,
    ...
    ), class = "tile_source")
}
