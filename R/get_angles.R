#' Elevation angle of scan(s)
#'
#' Gives the elevation angle of a scan, or the elevation angles within a
#' polar volume
#'
#' @param x A \code{pvol} or \code{scan} object.
#'
#' @return elevation in degrees
#'
#' @export
#' @examples
#' # load a polar volume
#' pvol <- system.file("extdata", "volume.h5", package="bioRad")
#' vol <- read.pvol(pvol)
#' # elevations for the scans in the volume
#' get_angles(vol)
#' # extract the first scan:
#' scan <- vol$scans[[1]]
#' # elevation angle of the scan:
#' get_angles(scan)
get_angles <- function(x) {
  UseMethod("get_angles", x)
}

#' @describeIn get_angles Elevation angle of a scan.
#' @export
get_angles.scan <- function(x) {
  stopifnot(inherits(x, "scan"))
  x$attributes$where$elangle
}

#' @describeIn get_angles Elevation angles of all scans in a polar volume.
#' @export
get_angles.pvol <- function(x) {
  stopifnot(inherits(x, "pvol"))
  sapply(x$scans, elangle.scan)
}
