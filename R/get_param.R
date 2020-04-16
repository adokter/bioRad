#' Get a scan parameter `param` from a scan `scan`
#'
#' @param x An object of class `scan`.
#' @param param A scan parameter.
#'
#' @return An object of class [param][summary.param].
#'
#' @export
#' @examples
#' # we will extract a scan parameter from the example scan object:
#' example_scan
#'
#' # extract the VRADH scan parameter
#' my_param <- get_param(example_scan, "VRADH")
#' my_param
get_param <- function(x, param) {
  assert_that(class(x) == "scan", msg = "`x` must be a scan object.")
  if (!(param %in% names(x$params))) stop(paste("Scan parameter", param, "not found in `x`."))
  x$params[[param]]
}
