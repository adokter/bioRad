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
#' vol=read.pvol(pvol)
#' # elevations for the scans in the volume
#' elangle(vol)
#' # extract the first scan:
#' scan=vol$scans[[1]]
#' # elevation angle of the scan:
#' elangle(scan)
elangle <- function(x) {
  UseMethod("elangle", x)
}

#' @describeIn elangle Elevation angle of a scan.
#' @export
elangle.scan <- function(x) {
  stopifnot(inherits(x, "scan"))
  x$attributes$where$elangle
}

#' @describeIn elangle Elevation angles of all scans in a polar volume.
#' @export
elangle.pvol <- function(x) {
  stopifnot(inherits(x, "pvol"))
  sapply(x$scans, elangle.scan)
}
