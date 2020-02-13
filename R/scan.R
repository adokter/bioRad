#' Class \code{scan}: a scan of a polar volume
#'
#' Class \code{scan} for a scan of a polar volume, and its associated R base functions.
#'
#' @param object Object of class \code{scan}
#' @param x Object of class \code{scan}
#' @param ... Additional arguments affecting the summary produced.
#'
#' @method summary scan
#'
#' @export
#'
#' @details
#' A object of class \code{scan} is a list containing:
#' \describe{
#'  \item{\code{radar}}{character string with the radar identifier}
#'  \item{\code{datetime}}{nominal time of the volume to which this
#'    scan belongs [UTC]}
#'  \item{\code{params}}{a list with scan parameters}
#'  \item{\code{attributes}}{list with the scan's \code{\\what},
#'    \code{\\where} and \code{\\how} attributes}
#'  \item{\code{geo}}{geographic data, a list with:
#'     \describe{
#'      \item{\code{lat}}{latitude of the radar [decimal degrees]}
#'      \item{\code{lon}}{longitude of the radar [decimal degrees]}
#'      \item{\code{height}}{height of the radar
#'        antenna [meters above sea level]}
#'      \item{\code{elangle}}{radar beam elevation [degrees]}
#'      \item{\code{rscale}}{range bin size [m]}
#'      \item{\code{ascale}}{azimuth bin size [deg]}
#'     }
#'     The \code{geo} element of a \code{scan} object is a copy of the
#'     \code{geo} element of its parent polar volume of class \code{pvol}.
#'   }
#' }
#'
#' @examples
#' # load example scan object
#' data(example_scan)
#'
#' # verify this object is of class scan:
#' is.scan(example_scan)
#'
#' # print the scan parameters contained in the scan:
#' example_scan$params
#'
#' # extract the VRADH scan parameter:
#' param <- get_param(example_scan, "VRADH")
#'
#' # print summary info for this scan parameter:
#' param
summary.scan <- function(object, ...) {
  print.scan(object)
}

#' @rdname summary.scan
#'
#' @return For \code{is.scan}: \code{TRUE} if its argument is of
#' class \code{scan}.
#'
#' @export
#'
#' @examples
#' is.scan("this is not a polar scan but a string") # > FALSE
is.scan <- function(x) {
  inherits(x, "scan")
}

#' @rdname summary.scan
#'
#' @return For \code{dim.scan}: dimensions of the scan.
#'
#' @export
dim.scan <- function(x) {
  stopifnot(inherits(x, "scan"))
  c(length(x$params), x$attributes$where$nbins, x$attributes$where$nrays)
}

#' Print method for class \code{scan}
#'
#' @param x An object of class \code{scan}, a polar scan.
#'
#' @keywords internal
#'
#' @export
print.scan <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  stopifnot(inherits(x, "scan"))
  cat("                  Polar scan (class scan)\n\n")
  cat("     parameters: ", names(x$params), "\n")
  cat("elevation angle: ", x$attributes$where$elangle, "deg\n")
  cat(
    "           dims: ", x$attributes$where$nbins, "bins x",
    x$attributes$where$nrays, "rays\n"
  )
}
