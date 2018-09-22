#' Get elevation angles of a polar volume (\code{pvol}) or scan (\code{scan})
#'
#' Gives the elevation angle of a single scan, or the elevation angles of all scans within a
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
#' vol <- read_pvolfile(pvol)
#' # elevations for the scans in the volume
#' get_elevation_angles(vol)
#' # extract the first scan:
#' scan <- vol$scans[[1]]
#' # elevation angle of the scan:
#' get_elevation_angles(scan)
get_elevation_angles <- function(x) {
  UseMethod("get_elevation_angles", x)
}

#' @describeIn get_elevation_angles Elevation angles of all scans in a polar volume.
#' @export
get_elevation_angles.pvol <- function(x) {
  stopifnot(inherits(x, "pvol"))
  sapply(x$scans, get_elevation_angles.scan)
}

#' @describeIn get_elevation_angles Elevation angle of a scan.
#' @export
get_elevation_angles.scan <- function(x) {
  stopifnot(inherits(x, "scan"))
  x$attributes$where$elangle
}

#' @describeIn get_elevation_angles Elevation angle of a scan parameter.
#' @export
get_elevation_angles.param <- function(x) {
  stopifnot(inherits(x, "param"))
  attributes(x)$geo$elangle
}
