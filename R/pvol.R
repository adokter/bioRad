#' Inspect a polar volume (`pvol`)
#'
#' R base functions for inspecting a polar volume (`pvol`) object.
#'
#' @param object A `pvol` object.
#' @param ... Additional arguments affecting the summary produced.
#'
#' @method summary pvol
#'
#' @export
#'
#' @details
#' A polar volume consists of a number of scans (or sweeps) made by the radar at
#' different elevation angles. A polar volume (`pvol`) object is a list
#' containing:
#' * `radar`: Radar identifier.
#' * `datetime`: Nominal time of the volume in UTC.
#' * `scans`: List of scans (`scan`) at different elevation angles.
#' * `attributes`: List of the volume's `what`, `where` and `how`
#' attributes.
#' * `geo`: List of the volume's geographic properties:
#'   * `lat`: Latitude of the radar in decimal degrees.
#'   * `lon`: Longitude of the radar in decimal degrees.
#'   * `height`: Height of the radar antenna in meters above sea level.
#'
#' @seealso
#' * [read_pvolfile()]
#' * [get_elevation_angles()]
#' * [get_scan()]
#'
#' @examples
#' # Locate and read the polar volume example file
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#' pvol <- read_pvolfile(pvolfile)
#'
#' # Verify that it is an object of class pvol
#' is.pvol(pvol)
#'
#' # Get summary info
#' pvol # Same as summary(pvol) or print(pvol)
#'
#' # Get dimensions
#' dim(pvol)
#'
#' # Get summary info for the scans in the polar volume
#' pvol$scans
summary.pvol <- function(object, ...) {
  print.pvol(object)
}

#' Print summary for an object of class `pvol`
#'
#' @noRd
#'
#' @export
print.pvol <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  stopifnot(inherits(x, "pvol"))
  cat("               Polar volume (class pvol)\n\n")
  cat("     # scans: ", length(x$scans), "\n")
  cat("       radar: ", x$radar, "\n")
  cat("      source: ", x$attributes$what$source, "\n")
  cat("nominal time: ", as.character(x$datetime), "\n\n")
}

#' Verify if an object is of class `pvol`
#'
#' @param x A `pvol` object.
#'
#' @return For [is.pvol()]: `TRUE` for an object of class `pvol`, otherwise
#'   `FALSE`.
#'
#' @rdname summary.pvol
#'
#' @export
is.pvol <- function(x) {
  inherits(x, "pvol")
}

#' Get dimensions for an object of class `pvol`
#'
#' @return For [dim.pvol()]: number of scans (`scan`) in a polar volume
#'   (`pvol`).
#'
#' @rdname summary.pvol
#'
#' @export
dim.pvol <- function(x) {
  stopifnot(inherits(x, "pvol"))
  c(length(x$scans))
}
