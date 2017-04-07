

#' Set/Get the Default Tile Cache Location
#'
#' The default tile cache location is the "rosm.cache" folder in the current
#' working directory, but for a variety of reasons it may be desirable to
#' use one cache directory for all calls in a script. This must be called
#' every time the namespace is loaded.
#'
#' @param cachedir A path to use as the cache directory (relative to the working directory).
#'   Use NULL to reset to the default.
#'
#' @return The previous cache directory, invisibly.
#' @export
#'
#' @examples
#' set_default_cachedir(tempfile())
#' get_default_cachedir()
#' (set_default_cachedir(NULL))
#'
set_default_cachedir <- function(cachedir) {
  # NULL sets back to the default
  if(is.null(cachedir)) {
    cachedir <- "rosm.cache"
  }
  if(!is.character(cachedir) || (length(cachedir) != 1)) {
    stop("'cachedir' must be a character vector of length 1")
  }
  # keep ref to old cachedir
  old_cachedir <- rosm_state$default_cachedir
  # set new cachedir
  rosm_state$default_cachedir <- cachedir
  # return old cachedir
  invisible(old_cachedir)
}


#' @rdname set_default_cachedir
#' @export
get_default_cachedir <- function() {
  rosm_state$default_cachedir
}

rosm_state <- new.env(parent = emptyenv())
rosm_state$default_cachedir <- "rosm.cache"


