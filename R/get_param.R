#' Get a scan parameter (\code{param}) from a scan (\code{scan})
#'
#' @param x An object of class \code{scan}.
#' @param param a scan parameter
#'
#' @return An object of class '\link[=summary.param]{param}'.
#'
#' @export
#' @examples
#' # we will extract a scan parameter from the example scan object:
#' example_scan
#' # extract the VRADH scan parameter
#' my_param <- get_param(example_scan, "VRADH")
#' my_param
get_param <- function(x, param) {
  stopifnot(inherits(x, "scan"))
  if (!(param %in% names(x$params))) stop(paste("scan parameter", param, "not found"))
  x$params[[param]]
}
