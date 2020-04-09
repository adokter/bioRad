#' Class \code{pvol}: a polar volume
#'
#' Class \code{pvol} for a polar volume, and its associated R base functions.
#'
#' @param object Object of class \code{pvol}.
#' @param x Object of class \code{pvol}.
#' @param ... Additional arguments affecting the summary produced.
#'
#' @method summary pvol
#'
#' @export
#'
#' @details
#' An object of class \code{pvol} is a list containing:
#' \describe{
#'  \item{\code{radar}}{character string with the radar identifier}
#'  \item{\code{datetime}}{nominal time of the volume (UTC)}
#'  \item{\code{scans}}{a list with scan objects of class 'scan'}
#'  \item{\code{attributes}}{list with the volume's \code{\\what},
#'    \code{\\where} and \code{\\how} attributes}
#'  \item{\code{geo}}{geographic data, a list with:
#'   \describe{
#'      \item{\code{lat}}{latitude of the radar (decimal degrees)}
#'      \item{\code{lon}}{longitude of the radar (decimal degrees)}
#'      \item{\code{height}}{height of the radar
#'        antenna (meters above sea level)}
#'   }
#'  }
#' }
#'
#' @examples
#' # locate example volume file:
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#'
#' # print the local path of the volume file:
#' pvolfile
#'
#' # load the file:
#' example_pvol <- read_pvolfile(pvolfile)
#'
#' # print summary info for the loaded polar volume:
#' example_pvol
#'
#' # verify that this is a pvol object:
#' is.pvol(example_pvol)
#'
#' # print summary info for the scans in the polar volume:
#' example_pvol$scans
#'
#' # copy the first scan to a new object 'scan':
#' scan <- example_pvol$scans[[1]]
#'
#' # print summary info for the scan:
#' scan
summary.pvol <- function(object, ...) {
  print.pvol(object)
}

#' @rdname summary.pvol
#'
#' @return for \code{is.pvol}: \code{TRUE} if its argument is of
#' class \code{pvol}
#'
#' @export
#'
#' @examples
#' is.pvol("this is not a polar volume but a string") # > FALSE
is.pvol <- function(x) inherits(x, "pvol")


#' Print method for class \code{pvol}
#'
#' @param x An object of class \code{pvol}, a polar volume
#'
#' @keywords internal
#'
#' @export
print.pvol <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  stopifnot(inherits(x, "pvol"))
  cat("               Polar volume (class pvol)\n\n")
  cat("     # scans: ", length(x$scans), "\n")
  cat("       radar: ", x$radar, "\n")
  cat("      source: ", x$attributes$what$source, "\n")
  cat("nominal time: ", as.character(x$datetime), "\n\n")
}
