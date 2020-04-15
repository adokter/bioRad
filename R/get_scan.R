#' Get a scan (`scan`) from a polar volume (`pvol`)
#'
#' Returns the scan with elevation angle closest
#' to `elev`.
#'
#' @param x An object of class `pvol`.
#' @param elev Elevation angle.
#'
#' @return An object of class [scan][summary.scan].
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
  assert_that(class(x) == class(pvol), msg = "`x` must be a pvol object.")
  x$scans[[which.min(abs(get_elevation_angles(x) - elev))]]
}
