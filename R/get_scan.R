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
#' scan_list <- get_scan(pvol, 3)
#' scan_list
#'
#' # Extract all scans closest to 1 degree elevation (0.5 and 1.5 degree scans)
#' scan_list <- get_scan(pvol,1, all=T)
#' scan_list
get_scan <- function(x, elev, all = FALSE) {
  assert_that(class(x) == "pvol", msg = "`x` must be a `pvol` object.")
  assert_that(class(elev) == "numeric", msg = "`elev` must be numeric.")
  assert_that(is.scalar(elev))
  assert_that(is.flag(all))
  difference_vector <- abs(get_elevation_angles(x) - elev)
  if(sum(min(difference_vector)==difference_vector)!=1 & !all)
  {
    warning("multiple elevation scans are equally close to `elev`, returning the first")
  }
  if(all){
    selection <- which(min(difference_vector)==difference_vector)
    return(x$scans[selection])
  }else{
    selection <- which.min(difference_vector)
    return(  x$scans[[selection]]
)
  }
}
