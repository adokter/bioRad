#' Get a scan (\code{scan}) from a polar volume (\code{pvol})
#'
#' @param x An object of class \code{pvol}.
#' @param elev Elevation angle.
#'
#' @return An object of class '\link[=summary.scan]{scan}'.
#'
#' @details The function returns the scan with elevation angle closest
#' to \code{elev}.
#'
#' @export
#' @examples
#' # locate example volume file:
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#'
#' # load the file:
#' example_pvol <- read_pvolfile(pvolfile)
#'
#' # extract the scan at 3 degree elevation:
#' scan <- get_scan(example_pvol, 3)
#'
#' # print summary info for this scan:
#' scan
get_scan <- function(x, elev) {
  stopifnot(inherits(x, "pvol"))
  x$scans[[which.min(abs(get_elevation_angles(x) - elev))]]
}
