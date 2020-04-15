#' Inspect a polar volume (`pvol`)
#'
#' R base functions for inspecting a polar volume (`pvol`) object.
#'
#' @param x A `pvol` object.
#' @param ... Additional arguments affecting the summary produced.
#'
#' @method summary pvol
#'
#' @export
#'
#' @details
#' A polar volume (`pvol`) object is a list containing:
#' * `radar`: Radar identifier.
#' * `datetime`: Nominal time of the volume (UTC).
#' * `scans`: List of scans (`scan`) at different elevation angles.
#' * `attributes`: List of the the volume's `what`, `where` and `how`
#' attributes.
#' * `geo`: List of the volume's geographic properties:
#'   * `lat`: Latitude of the radar (decimal degrees).
#'   * `lon`: Longitude of the radar (decimal degrees).
#'   * `height`: Height of the radar antenna (meters above sea level).
#'
#' @seealso
#' * [read_pvolfile()] to read polar volume files.
#' * [summary.scan()] to inspect scans.
#'
#' @examples
#' # Locate the polar volume example file:
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#'
#' # Read the file:
#' example_pvol <- read_pvolfile(pvolfile)
#'
#' # Verify that it is a pvol object:
#' is.pvol(example_pvol)
#'
#' # Print summary info:
#' example_pvol # Same as summary(example_pvol) or print(example_pvol)
#'
#' # Print summary info for the scans in the polar volume:
#' example_pvol$scans
#'
#' # Copy the first scan to a new object `scan`:
#' scan <- example_pvol$scans[[1]]
#'
#' # Print summary info for the scan:
#' scan # Same as summary(scan) or print(scan)
summary.pvol <- function(x, ...) {
  print.pvol(x)
}

#' Verify if an object is of class `pvol`
#'
#' @inheritParams summary.pvol
#'
#' @return For `is.pvol(x)`: `TRUE` if `x` is an object of class `pvol`,
#'   otherwise `FALSE`.
#'
#' @rdname summary.pvol
#'
#' @export
is.pvol <- function(x) inherits(x, "pvol")

#' Print summary for an object of class `pvol`
#'
#' @inheritParams summary.pvol
#'
#' @rdname summary.pvol
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
