#' Get a scan (\code{scan}) from a polar volume (\code{pvol})
#'
#' @param x An object of class \code{pvol}.
#' @param angle Elevation angle.
#'
#' @return An object of class '\link[=summary.scan]{scan}'.
#'
#' @details The function returns the scan with elevation angle closest
#' to \code{angle}.
#'
#' @export
#' @examples
#' # locate example volume file:
#' pvol <- system.file("extdata", "volume.h5", package="bioRad")
#' # load the file:
#' vol <- read_pvolfile(pvol)
#' # extract the scan at 3 degree elevation:
#' myscan <- get_scan(vol,3)
get_scan <- function(x, angle) {
  stopifnot(inherits(x, "pvol"))
  x$scans[[which.min(abs(get_elevation_angles(x) - angle))]]
}
