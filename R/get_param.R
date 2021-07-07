#' Get a parameter (`param`) from a scan (`scan`)
#'
#' Returns the selected parameter (`param`) from a scan (`scan`).
#'
#' @param x A `scan` object.
#' @param param Character. A scan parameter, e.g. `DBZH` or `VRADH`. See
#'   [summary.param()] for commonly available parameters.
#'
#' @return A `param` object.
#'
#' @export
#'
#' @seealso
#' * [summary.param()]
#'
#' @examples
#' # Get summary info for a scan (including parameters)
#' example_scan
#'
#' # Extract the VRADH scan parameter
#' param <- get_param(example_scan, "VRADH")
#'
#' # Get summary info for this parameter
#' param
get_param <- function(x, param) {
  assert_that(class(x) == "scan", msg = "`x` must be a scan object.")
  if (!(param %in% names(x$params))) stop(
    glue("Can't find parameter `{param}` in `x`.")
  )
  x$params[[param]]
}
