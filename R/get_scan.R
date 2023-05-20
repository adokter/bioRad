#' Get a scan (`scan`) from a polar volume (`pvol`)
#'
#' Returns the scan (`scan`) from a polar volume (`pvol`) with elevation angle
#' closest to `elev`.
#'
#' @param x A `pvol` object.
#' @param elev Numeric. Elevation angle in degrees.
#' @param all Logical. Return the first scan in the `pvol` object
#' closest to the requested elevation (`FALSE`), or a list with
#' all scans equally close to the requested elevation (`TRUE`).
#'
#' @return A `scan` object when `all` equals `FALSE` (default), or a list of `scan` objects if `all` equals `TRUE`
#'
#' @export
#'
#' @details In cases where `elev` is exactly in between two
#' scan elevation angles, the lower elevation angle scan is returned.
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
#' # Extract the scan closest to 3 degrees elevation (2.5 degree scan)
#' scan <- get_scan(pvol, 3)
#'
#' # Get summary info
#' scan
#'
#' # Extract all scans closest to 3 degrees elevation (2.5 degree scan)
#' # Always returns a list with scan object(s), containing multiple scans
#' # if the pvol contains multiple scans at the same closest elevation.
#' scan_list <- get_scan(pvol, 3)
#' scan_list
get_scan <- function(x, elev, all = FALSE) {
  assertthat::assert_that(class(x) == "pvol", msg = "`x` must be a `pvol` object.")
  assertthat::assert_that(class(elev) == "numeric", msg = "`elev` must be numeric.")
  assertthat::assert_that(assertthat::is.scalar(elev))
  assertthat::assert_that(assertthat::is.flag(all))
  difference_vector <- abs(get_elevation_angles(x) - elev)
  # select indices closest to elev:
  selection <- which(min(difference_vector)==difference_vector)
  # of those, select the ones at lowest elevation
  selection <- which(min(get_elevation_angles(x)[selection]) == get_elevation_angles(x))

  if(length(selection)>1 & !all)
  {
    warning("multiple scans at the same elevation, returning the first")
  }
  if(all){
    return(x$scans[selection])
  }else{
    return(x$scans[[selection[1]]])
  }
}
