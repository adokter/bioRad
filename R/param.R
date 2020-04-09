#' Class \code{param}: a parameter of a scan of a polar volume
#'
#' Class \code{param} for a parameter of a scan of a polar volume, and its
#' associated R base functions.
#'
#' @param object Object of class \code{param}.
#' @param x Object of class \code{param}.
#' @param ... Additional arguments affecting the summary produced.
#'
#' @method summary param
#'
#' @details
#' An object of class \code{scan} is a simple matrix, with the following
#' specific attributes:
#' \describe{
#'    \item{\code{radar}}{character string with the radar identifier}
#'    \item{\code{datetime}}{nominal time of the volume to which this
#'      scan belongs (UTC)}
#'    \item{\code{lat}}{latitude of the radar (decimal degrees)}
#'    \item{\code{lon}}{longitude of the radar (decimal degrees)}
#'    \item{\code{height}}{height of the radar antenna (meters above sea level)}
#'    \item{\code{get_elevation_angles}}{radar beam elevation (degrees)}
#'    \item{\code{param}}{string with the name of the polar scan parameter}
#' }
#' Scan parameters are named according to the OPERA data information model
#' (ODIM), see Table 16 in the
#' \href{https://github.com/adokter/vol2bird/blob/master/doc/OPERA2014_O4_ODIM_H5-v2.2.pdf}{ODIM specification}.
#' Commonly available parameters are:
#' \describe{
#'  \item{"\code{DBZH}", "\code{DBZ}"}{(Logged) reflectivity factor (dBZ)}
#'  \item{"\code{VRADH}", "\code{VRAD}"}{Radial velocity (m/s). Radial
#'    velocities towards the radar are negative, while radial velocities away
#'    from the radar are positive}
#'  \item{"\code{RHOHV}"}{Correlation coefficient (unitless). Correlation
#'    between vertically polarized and horizontally polarized
#'    reflectivity factor}
#'  \item{"\code{PHIDP}"}{Differential phase (degrees)}
#'  \item{"\code{ZDR}"}{(Logged) differential reflectivity (dB)}
#' }
#' @examples
#' # load example scan object
#' data(example_scan)
#'
#' # extract the DBZH scan parameter:
#' param <- get_param(example_scan, "DBZH")
#'
#' # verify this is an object of class param:
#' is.param(param)
#'
#' # print summary info for this scan parameter:
#' param
summary.param <- function(object, ...) {
  print.param(object)
}

#' @rdname summary.param
#'
#' @return for \code{is.scan}: \code{TRUE} if its argument is of
#' class "\code{param}"
#'
#' @export
is.param <- function(x) {
  inherits(x, "param")
}

#' Print method for class \code{param}
#'
#' @param x An object of class \code{param}, a polar scan parameter.
#'
#' @keywords internal
#'
#' @export
print.param <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  stopifnot(inherits(x, "param"))
  cat("               Polar scan parameter (class param)\n\n")
  cat("    quantity: ", attributes(x)$param, "\n")
  cat("        dims: ", dim(x)[1], "bins x", dim(x)[2], "rays\n")
}
