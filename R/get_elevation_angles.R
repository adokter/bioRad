#' Get elevation angles of a polar volume (`pvol`), scan (`scan`) or parameter
#' (`param`)
#'
#' Gives the elevation angles of all scans within a polar volume (`pvol`) or the
#' elevation angle of a single scan (`scan`) or scan parameter (`param`) in
#' degrees.
#'
#' @param x A `pvol`, `scan` or `param` object.
#'
#' @return The elevation angle(s) in degrees.
#'
#' @export
#'
#' @examples
#' # Locate and read the polar volume example file:
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#' example_pvol <- read_pvolfile(pvolfile)
#'
#' # Get the elevations angles of the scans in the pvol:
#' get_elevation_angles(example_pvol)
#'
#' # Extract the first scan:
#' scan <- example_pvol$scans[[1]]
#'
#' # Get the elevation angle of that scan:
#' get_elevation_angles(scan)
get_elevation_angles <- function(x) {
  UseMethod("get_elevation_angles", x)
}

#' @rdname get_elevation_angles
#'
#' @export
get_elevation_angles.pvol <- function(x) {
  stopifnot(inherits(x, "pvol"))
  sapply(x$scans, get_elevation_angles.scan)
}

#' @rdname get_elevation_angles
#'
#' @export
get_elevation_angles.scan <- function(x) {
  stopifnot(inherits(x, "scan"))
  x$attributes$where$elangle
}

#' @rdname get_elevation_angles
#'
#' @export
get_elevation_angles.param <- function(x) {
  stopifnot(inherits(x, "param"))
  attributes(x)$geo$elangle
}
