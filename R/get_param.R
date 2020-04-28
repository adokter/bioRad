#' Get a parameter (`param`) from a scan (`scan`)
#'
#' Returns the selected parameter (`param`) from a scan (`scan`).
#'
#' @param x A `scan` object.
#' @param param Character. A scan parameter, such as `DBZH` or `VRADH`. See
#'   [summary.param()] for commonly available parameters.
#'
#' @return A `param` object.
#'
#' @export
#'
#' @seealso [summary.param()]
#'
#' @examples
#' # Load the example scan
#' scan <- example_scan
#'
#' # Get summary info (including parameters)
#' scan
#'
#' # Extact the VRADH scan parameter
#' param <- get_param(scan, "VRADH")
#'
#' # Get summary info for this parameter
#' param
get_param <- function(x, param) {
  assert_that(class(x) == "scan", msg = "`x` must be a scan object.")
  if (!(param %in% names(x$params))) stop(paste0("Can't find parameter `", param, "` in `x`."))
  x$params[[param]]
}
