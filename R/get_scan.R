#' Get a scan (`scan`) from a polar volume (`pvol`)
#'
#' Returns the scan (`scan`) from a polar volume (`pvol`) with elevation angle
#' closest to `elev`.
#'
#' @param x A `pvol` object.
#' @param elev Numeric. Elevation angle.
#' @param all Logical. Whether to return all scans with a minimal difference in elevation angle as a list or only the first scan as a scan
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
get_scan <- function(x, elev, all = FALSE) {
  assert_that(class(x) == "pvol", msg = "`x` must be a `pvol` object.")
  assert_that(class(elev) == "numeric", msg = "`elev` must be numeric.")
  assert_that(is.scalar(elev))
  assert_that(is.flag(all))
  difference_vector <- abs(get_elevation_angles(x) - elev)
  if(sum(min(difference_vector)==difference_vector)!=1 & !all)
  {
    warning("There are multiple elevation scans with the same difference to elev therefore the first is taken")
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
