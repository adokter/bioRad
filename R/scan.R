#' Inspect a scan (`scan`)
#'
#' R base functions for inspecting a scan (`scan`) object.
#'
#' @param x A `scan` object.
#' @param ... Additional arguments affecting the summary produced.
#'
#' @method summary scan
#'
#' @export
#'
#' @details
#' A scan (or sweep) is made by the radar at a certain elevation angle. The
#' resulting parameter data (`param`) are organized along radar range (bins) and
#' azimuth (rays). A scan (`scan`) object is a list containing:
#' * `radar`: Radar identifier.
#' * `datetime`: Nominal time of the volume to which the scan belongs in UTC.
#' * `params`: List of scan parameters (`param`).
#' * `attributes`: List of the scan's `what`, `where` and `how`
#' attributes.
#' * `geo`: List of the scan's geographic properties:
#'   * `lat`: Latitude of the radar in decimal degrees.
#'   * `lon`: Longitude of the radar in decimal degrees.
#'   * `height`: Height of the radar antenna in meters above sea level.
#'   * `elange`: Elevation angle of the radar beam for that scan in degrees.
#'   * `rscale`: Range bin size for that scan in m (e.g. 500 m * 480 bins equals
#'   240 km range).
#'   * `ascale`: Azimuth bin size for that scan in degrees (e.g. 1 degree * 360
#'   rays equals full circle).
#'
#' @seealso
#' * [get_scan()]
#' * [`example_scan`]
#' * [get_param()]
#'
#' @examples
#' # Load example scan
#' data(example_scan)
#'
#' # Verify that is an object of class scan
#' is.scan(example_scan)
#'
#' # Get summary info
#' example_scan # Same as summary(example_scan) or print(example_scan)
#'
#' # Get dimensions
#' dim(example_scan)
#'
#' # Get summary info for the parameters in the scan
#' example_scan$params
summary.scan <- function(x, ...) {
  print.scan(x)
}

#' Verify if an object is of class `scan`
#'
#' @inheritParams summary.scan
#'
#' @return For [is.scan()]: `TRUE` for an object of class `scan`, otherwise
#'   `FALSE`.
#'
#' @rdname summary.scan
#'
#' @export
is.scan <- function(x) {
  inherits(x, "scan")
}

#' Get dimensions for an object of class `scan`
#'
#' @return For [dim.scan()]: number of parameters, bins and rays in a scan.
#'
#' @rdname summary.scan
#'
#' @export
dim.scan <- function(x) {
  stopifnot(inherits(x, "scan"))
  c(length(x$params), x$attributes$where$nbins, x$attributes$where$nrays)
}

#' Print summary for an object of class `scan`
#'
#' @inheritParams summary.scan
#'
#' @rdname summary.scan
#'
#' @export
print.scan <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  stopifnot(inherits(x, "scan"))
  cat("                  Polar scan (class scan)\n\n")
  cat("     parameters: ", names(x$params), "\n")
  cat("elevation angle: ", x$attributes$where$elangle, "deg\n")
  cat(
    "           dims: ", x$attributes$where$nbins, "bins x",
    x$attributes$where$nrays, "rays\n"
  )
}
