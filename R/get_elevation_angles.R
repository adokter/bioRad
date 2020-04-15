#' Get elevation angles of a polar volume (`pvol`) or scan (`scan`)
#'
#' Gives the elevation angle of a single scan, or the elevation angles of all scans within a
#' polar volume
#'
#' @param x A `pvol`, `scan` or `scan parameter`.
#'
#' @return Elevation in degrees.
#'
#' @export
#' @examples
#' # load a polar volume
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#' example_pvol <- read_pvolfile(pvolfile)
#'
#' # elevations for the scans in the volume
#' get_elevation_angles(example_pvol)
#'
#' # extract the first scan:
#' scan <- example_pvol$scans[[1]]
#'
#' # elevation angle of the scan:
#' get_elevation_angles(scan)
get_elevation_angles <- function(x) {
  UseMethod("get_elevation_angles", x)
}

#' @rdname get_elevation_angles
#' @export
get_elevation_angles.pvol <- function(x) {
  stopifnot(inherits(x, "pvol"))
  sapply(x$scans, get_elevation_angles.scan)
}

#' @rdname get_elevation_angles
#' @export
get_elevation_angles.scan <- function(x) {
  stopifnot(inherits(x, "scan"))
  x$attributes$where$elangle
}

#' @rdname get_elevation_angles
#' @export
get_elevation_angles.param <- function(x) {
  stopifnot(inherits(x, "param"))
  attributes(x)$geo$elangle
}
