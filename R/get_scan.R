#' Get a scan (`scan`) from a polar volume (`pvol`)
#'
#' Returns the scan with elevation angle closest to `elev`.
#'
#' @param x A `pvol` object.
#' @param elev Numeric. Elevation angle.
#'
#' @return A `scan` object.
#'
#' @export
#'
#' @seealso
#' * [summary.scan()]
#' * [get_elevation_angles()]
#'
#' @examples
#' # Locate and read the polar volume example file
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#' pvol <- read_pvolfile(pvolfile)
#'
#' # Get elevation angles
#' get_elevation_angles(pvol)
#'
#' # Extract the scan closest to 3 degrees elevation (= 2.5)
#' scan <- get_scan(pvol, 3)
#'
#' # Get summary info
#' scan
get_scan <- function(x, elev) {
  assert_that(class(x) == "pvol", msg = "`x` must be a pvol object.")
  assert_that(class(elev) == "numeric", msg = "`elev` must be numeric.")
  x$scans[[which.min(abs(get_elevation_angles(x) - elev))]]
}
