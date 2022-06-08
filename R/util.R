
#' Check for Internet
#'
#' @return TRUE if the internet is available, false otherwise
#' @export
#'
#' @examples
#' has_internet()
#'
has_internet <- function() {
  !is.null(curl::nslookup("r-project.org", error = FALSE))
}
